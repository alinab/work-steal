(* The following types signify:
   - Value wraps an item popped or stolen from the deque
   - Abort signifies an unsucessful attempt at stealing from the deque
   - Empty signifies that the deque is empty
*)

(* let capacity_hist = Array.init 32 (fun _ -> Atomic.make 0) *)
type 'a result =
    | Empty
    | Abort
    | Value of 'a

type pool_mode = GC | Pool

type shrink_policy =
  | No_shrink
  | Simple
  | No_copy
  | Multi

(* ------------------------------------------------------------------ *
 *  Shared buffer pool — a global Treiber stack per log_size bucket.
 *  The owner pushes old segments back when shrinking and pops them
 *  when growing, avoiding heap allocation and GC pressure.
 * ------------------------------------------------------------------ *)
let max_log_size = 32

(* The pool stores segments as (unit option Atomic.t array) to avoid
   the value restriction on polymorphic mutable globals. Obj.magic is
   used only at push/pop boundaries; correctness is guaranteed because
   each bucket k only ever holds segments of log_size = k. *)
type pool_node = {
  segment : unit option Atomic.t array;
  next    : pool_node option Atomic.t;
}

let pool : pool_node option Atomic.t array =
  Array.init max_log_size (fun _ -> Atomic.make None)

let pool_push log_size seg =
  let node = { segment = Obj.magic seg; next = Atomic.make None } in
  let rec loop () =
    let old_head = Atomic.get pool.(log_size) in
    Atomic.set node.next old_head;
    if not (Atomic.compare_and_set pool.(log_size) old_head (Some node))
    then loop ()
  in
  loop ()

let pool_pop log_size =
  let rec loop () =
    match Atomic.get pool.(log_size) with
    | None -> None
    | Some node as old_head ->
        let new_head = Atomic.get node.next in
        if Atomic.compare_and_set pool.(log_size) old_head new_head
        then Some (Obj.magic node.segment)
        else loop ()
  in
  loop ()

(* The deque is represented by:
    - an index (bottom) to which the next item is pushed to.
    - an index (top) from which an item is stolen after which it
    is incremented.
    - a circular array of where the entire array is stored within
      an atomic reference named as the active_array.
      --- ADD NOTE on the Atomic
*)
type 'a t = {
    bottom       : int Atomic.t;
    top          : int Atomic.t;
    active_array : 'a Circular_array.t Atomic.t;
    grows        : int Atomic.t;
    shrinks      : int Atomic.t;
    max_len      : int Atomic.t;
    min_len      : int Atomic.t;
}

let create log_initial_size = {
    bottom       = Atomic.make 0;
    top          = Atomic.make 0;
    active_array = Atomic.make (Circular_array.create log_initial_size);
    grows        = Atomic.make 0;
    shrinks      = Atomic.make 0;
    max_len      = Atomic.make 0;
    min_len      = Atomic.make max_int;
}
(* Stealing an item from the deque is managed by a CAS (Compare and Set
   operation. Given that the top index is never decremented,
   if the top index has changed, it can only be by having another process
   steal an item from the deque before the current domain can do so.
   If top has not changed, the item at top can be stolen by the current
   domain attempting a steal operation.
*)
let cas_top deque_arr old_top new_top =
    Atomic.compare_and_set deque_arr.top old_top new_top

(* ------------------------------------------------------------------ *
 *  push_bottom — reuse a pooled segment when growing if available(* Pushes an item into the deque. The important check is that
   if the size of the deque is greater than the size of the underlying
   circular array, then the array is grown and the newly expanded array
   set to be the underlying one.
   --- ADD NOTE on the (size - 1).
   The bottom index is incremented to point to the next slot to which
   an item can be pushed to.
   GC : Directly allocates new arrays from memory
   Pool : checks if allocation can be done from pool first, if not then from memory
*)
 * ------------------------------------------------------------------ *)
let push_bottom deq task ~pool_mode =
   let b = Atomic.get deq.bottom in
   let t = Atomic.get deq.top in
   let a = Atomic.get deq.active_array in
   let a =
    if b - t >= Circular_array.size a - 1
    then begin
      let new_log = a.Circular_array.log_size + 1 in
      let new_a =
        match pool_mode with
        | GC -> Circular_array.grow a ~bottom:b ~top:t
        | Pool ->
            match pool_pop new_log with
            | Some seg ->
                Circular_array.create_with_segment new_log seg ~bottom:b ~top:t ~src:a
            | None ->
                Circular_array.grow a ~bottom:b ~top:t
      in
      Atomic.incr deq.grows;
      Atomic.set deq.active_array new_a;
      Atomic.set deq.max_len (max (Atomic.get deq.max_len) (Circular_array.size new_a));
      new_a
    end else
      a
   in
   Circular_array.put_item a b task;
   Atomic.set deq.bottom (b + 1)


(* Steals the item at the top index of the deque. If the CAS at top
   fails, another process already stole the item otherwise the stolen
   value is returned.

   Additional logic for pool : do_shrink_shared_pool offsets the top and bottom by new size,
   if it happens in between the read of top and bottom, no of elements live are incorrect,
   so a check is added to see if top is the same as what was read before and array is unchanged.
*)
let steal deq =
    let t       = Atomic.get deq.top in
    let old_arr = Atomic.get deq.active_array in   (* 11.1: before bottom *)
    let b       = Atomic.get deq.bottom in
    let a       = Atomic.get deq.active_array in
    let size    = b - t in
    if size <= 0 then Empty
    else if size mod (Circular_array.size a) = 0 then begin
      if a == old_arr && t = Atomic.get deq.top
      then Empty
      else Abort
    end
    else begin
      let stolen_task = Circular_array.get_item a t in
      if not (cas_top deq t (t + 1))
      then Abort
      else Value stolen_task
    end

(* ------------------------------------------------------------------ *
 *  do_shrink_shared_pool — the bottom/top increment dance (Figure 7)
 * ------------------------------------------------------------------ *)
let do_shrink_shared_pool deq ~bottom ~old_a ~new_a =
  let new_size = Circular_array.size new_a in
  Atomic.set deq.active_array new_a;                              (* 39.1 *)
  Atomic.set deq.bottom (bottom + new_size);                      (* 39.3 *)
  let t = Atomic.get deq.top in                                   (* 39.4 *)
  if not (Atomic.compare_and_set deq.top t (t + new_size)) then  (* 39.5 *)
    Atomic.set deq.bottom bottom;                                 (* 39.6 *)
  pool_push old_a.Circular_array.log_size                        (* 39.7 *)
            old_a.Circular_array.segment;
  Atomic.incr deq.shrinks;
  Atomic.set deq.min_len (min (Atomic.get deq.min_len) new_size)



  

let do_shrink_gc deq ~bottom:_ ~old_a:_ ~new_a =
  Atomic.set deq.active_array new_a;
  Atomic.incr deq.shrinks;
  Atomic.set deq.min_len (min (Atomic.get deq.min_len) (Circular_array.size new_a))
  (* old array becomes unreachable; GC collects it once no thief holds a reference *)

  (* Constant used for shrinking the array *)
let k = 3

(* Shrinks the underlying circular array if the number of elements
   is less than array size / K where K >= 3. The underlying circular
   array is always shrunk to half its size which is why the the value
   of K >= 3 is valid: (array_size / 3) < (array_size < 2). Also,
   after shrinking, the deque or the underlying circular array should
   have slots to push elements otherwise the need to grow the array will
   almost immediately arise.
*)
let perhaps_shrink_simple deq ~bottom ~top ~pool_mode =
  let a = Atomic.get deq.active_array in
  let curr_size = Circular_array.size a in
  if curr_size > 1 && bottom - top < curr_size / k then begin
    let new_a = Circular_array.shrink a ~bottom ~top in
    match pool_mode with
    | GC   -> do_shrink_gc   deq ~bottom ~old_a:a ~new_a
    | Pool -> do_shrink_shared_pool deq ~bottom ~old_a:a ~new_a
  end
    

(* ------------------------------------------------------------------ *
 *  VERSION 2 – shrink without (full) copying
 *
 *  Strategy:
 *    • When growing we stored a back-pointer (a.prev) to the smaller
 *      array and a low_water_mark recording the lowest `bottom` index
 *      ever written into the big array while it was active.
 *    • On shrink we reuse that smaller array.  Only indices in the
 *      range [lwm, bottom) were mutated while the big array was live,
 *      so only those slots need to be copied back.
 *    • We also update the smaller array's lwm to
 *        min(small.lwm, big.lwm)
 *      so that a future shrink from small knows the full mutation
 *      history.
 *
 *  Corner case: if there is no prev (we're already at the root array)
 *  fall back to the simple allocating shrink.
 * ------------------------------------------------------------------ *)  
let perhaps_shrink_no_copy deq ~bottom ~top ~pool_mode =
  let a            = Atomic.get deq.active_array in
  let num_elements = bottom - top in
  let curr_size    = Circular_array.size a in
  if curr_size > 1 && num_elements < curr_size / k then begin
    let do_shrink = match pool_mode with
      | GC   -> do_shrink_gc
      | Pool -> do_shrink_shared_pool
    in
    match Atomic.get a.Circular_array.prev with
    | None ->
        let new_a = Circular_array.shrink a ~bottom ~top in
        do_shrink deq ~bottom ~old_a:a ~new_a
    | Some smaller ->
        let lwm = Atomic.get a.Circular_array.low_water_mark in
        let lo  = if lwm = max_int then top else max top lwm in
        for i = lo to bottom - 1 do
          Circular_array.put_item smaller i (Circular_array.get_item a i)
        done;
        Atomic.set smaller.Circular_array.low_water_mark
          (min (Atomic.get smaller.Circular_array.low_water_mark) lo);
        do_shrink deq ~bottom ~old_a:a ~new_a:smaller
  end
(* ------------------------------------------------------------------ *
 *  VERSION 3 – combined multiple shrinks
 *
 *  Strategy:
 *    Walk the chain of back-pointers (a → a.prev → a.prev.prev → …)
 *    to find the smallest ancestor whose size can still hold all the
 *    live elements (i.e. size > num_elements, with one cell unused per
 *    the paper).  Track the minimum LWM across every skipped array.
 *
 *    Then copy only the slots in [global_lwm, bottom) from the current
 *    big array into the chosen ancestor.  This is correct because:
 *      – slots in [top, global_lwm) were never written while any of the
 *        skipped arrays were active, so the ancestor array still holds
 *        their correct values from when it was last live.
 *      – slots in [global_lwm, bottom) might have been overwritten;
 *        we copy those from the newest (largest) array which has the
 *        most recent values.
 *
 *  Falls back to allocating shrink if no suitable ancestor exists.
 * ------------------------------------------------------------------ *)
let perhaps_shrink_multi deq ~bottom ~top ~pool_mode =
  let a            = Atomic.get deq.active_array in
  let num_elements = bottom - top in
  let curr_size    = Circular_array.size a in
  if curr_size > 1 && num_elements < curr_size / k then begin
    let do_shrink = match pool_mode with
      | GC   -> do_shrink_gc
      | Pool -> do_shrink_shared_pool
    in
    let global_lwm = ref (Atomic.get a.Circular_array.low_water_mark) in
    let target = ref None in
    let cur    = ref (Some a) in
    let stop   = ref false in
    while not !stop do
      match !cur with
      | None -> stop := true
      | Some node ->
          (match Atomic.get node.Circular_array.prev with
           | None -> stop := true
           | Some prev ->
               if Circular_array.size prev > num_elements then begin
                (* prev can hold all elements — it's a candidate target.
                    Accumulate node's lwm since we are skipping past node. *)
                 global_lwm := min !global_lwm
                   (Atomic.get node.Circular_array.low_water_mark);
                 target := Some prev;
                 cur    := Some prev
               end else
                 stop := true)
    done;
    match !target with
    | None ->
        let new_a = Circular_array.shrink a ~bottom ~top in
        do_shrink deq ~bottom ~old_a:a ~new_a
    | Some dest ->
        let raw_lwm = !global_lwm in
        let lo = if raw_lwm = max_int then top else max top raw_lwm in
        for i = lo to bottom - 1 do
          Circular_array.put_item dest i (Circular_array.get_item a i)
        done;
        Atomic.set dest.Circular_array.low_water_mark
          (min (Atomic.get dest.Circular_array.low_water_mark) lo);
        do_shrink deq ~bottom ~old_a:a ~new_a:dest
  end

(* ------------------------------------------------------------------ *
 *  pop_bottom — unchanged logic; shrink variants now use pool dance
 * ------------------------------------------------------------------ *)

(* Pops an item from the bottom of the deque. With the bottom
   index always incremented by 1 after a push, pop must first
   decrement the index by 1 to obtain an element to pop. This is
   set to be the new index for bottom.

   With bottom already decremented, if top is greater than bottom,
   it means that the deque is empty with either elements stolen from
   top or popped from bottom. For this, the size of the deque is checked
   to be < 0 and bottom = top to signal an empty deque

   After an element is popped, the underlying circular array is
   heuristically shrunk using a factor and the perhaps_shrink method.
   If this were not done, then the deque memory usage would depend on the
   maximum of the size to which the circular array grows.

   Only if size = 1, a CAS is required. With another process racing with
   the deque to steal the last remaining element, if the CAS fails, then the
   item is returned wrapped in a Value; otherwise an Empty signifies that
   another process stole the element. The bottom index is set to
   *)
let pop_bottom ?(shrink = Simple) ?(pool_mode = GC) deq =
    let b = Atomic.get deq.bottom - 1 in
    Atomic.set deq.bottom b;
    let t = Atomic.get deq.top in
    let size = b - t in
    if (size < 0) then begin
        Atomic.set deq.bottom t;
        Empty
    end
    else begin
      (* read active array here to ensure that any changes in
         array size are accurately reflected *)
      let a    = Atomic.get deq.active_array in
      let task = Circular_array.get_item a b in
      if (size > 0) then begin
        (match shrink with
          | No_shrink -> ()
          | Simple    -> perhaps_shrink_simple  deq ~bottom:b ~top:t ~pool_mode:pool_mode
          | No_copy   -> perhaps_shrink_no_copy deq ~bottom:b ~top:t ~pool_mode:pool_mode
          | Multi     -> perhaps_shrink_multi   deq ~bottom:b ~top:t ~pool_mode:pool_mode);
        ignore(Atomic.get deq.active_array);(* ensure shrink is visible before returning *)
        Value task
      end
      (* size = 1 i.e. popping the last remaining element races with thieves
         trying to steal the same element *)
      else begin
          let result =
              if cas_top deq t (t + 1)
              then Value task
              else Empty (* some thief stole the last element *)
          in
          (* bottom = top i.e. deque is now empty *)
          Atomic.set deq.bottom (t + 1);
          result
      end
   end

(* Benchmarking accessors *)
let grows   d = d.grows
let shrinks d = d.shrinks
let max_len d = d.max_len
let min_len d = d.min_len