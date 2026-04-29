type 'a t = {
    log_size : int;
    (* Exponent of array size — stored as log rather than size directly
       to enable O(1) grow/shrink via increment/decrement and fast
       cyclic indexing via 1 lsl log_size *)
    segment: 'a option Atomic.t array;
    (* Atomic slots required — owner and thieves may concurrently access
       different indices, and Atomic.set/get provides the necessary
       memory ordering guarantees *)
    prev     : 'a t option Atomic.t;
    (* Version 2/3: back-pointer to the smaller array this was grown from.
       None for the root array created by [create].                        *)
    (* Version 2/3: lowest index of [bottom] written via [put_item] while
       this array was active. Starts at [max_int] and is updated on every
       [put_item] call. Used to determine which slots need copying on shrink. *)
    low_water_mark : int Atomic.t;

}

let create log_size = {
    log_size = log_size;
    segment = Array.init (1 lsl log_size) (fun _ -> Atomic.make None);
    prev = Atomic.make None;
    low_water_mark = Atomic.make max_int;
}

let size _array = 1 lsl (_array.log_size)

let get_item _array idx =
    let size = size _array in
    let mod_idx = idx mod size in
    match Atomic.get _array.segment.(mod_idx) with
    | Some v -> v
    | None -> failwith "Circular_array.get: uninitialized slot"
     (* None should never occur in correct usage — top and bottom
       invariants guarantee only previously written slots are read *)

let put_item _array idx item =
    let size = size _array in
    (* Track the lowest bottom ever stored into this array.
       Only the owner thread calls put_item so a plain fetch-and-update
       (read then set) is safe here without a CAS loop.            *)
    let lwm = Atomic.get _array.low_water_mark in
    if idx < lwm then Atomic.set _array.low_water_mark idx;
    let mod_idx = idx mod size in
    Atomic.set _array.segment.(mod_idx) (Some item)

(* grow: double the size, attach back-pointer, copy all live elements.
   The new array's lwm starts at [bottom] — the first slot that will
   be written into it by the subsequent [put_item] in [push_bottom].  *)
let grow _array ~bottom ~top =
    let new_array = {
        log_size       = _array.log_size + 1;
        segment        = Array.init (1 lsl (_array.log_size + 1)) (fun _ -> Atomic.make None);
        prev           = Atomic.make (Some _array);               (* ← back-pointer *)
        low_water_mark = Atomic.make bottom;   (* first write will be here *)
    } in

    for i = top to bottom - 1 do
        put_item new_array i (get_item _array i)
    done;
    new_array

let shrink _array ~bottom ~top =
    let shrunk_array = {
        log_size       = _array.log_size - 1;
        segment        = Array.init (1 lsl (_array.log_size - 1)) (fun _ -> Atomic.make None);
        prev           = Atomic.make None;   (* fresh array, no ancestry chain *)
        low_water_mark = Atomic.make max_int;
    } in
    for i = top to bottom - 1 do
        put_item shrunk_array i (get_item _array i)
    done;
    shrunk_array

(* Used when growing with a recycled segment from the shared pool.
   Clears all slots in the recycled segment, then copies live elements
   from src, mirroring what [grow] does but without allocating. *)
let create_with_segment log_size seg ~bottom ~top ~src =
  (* clear all slots — the recycled segment may have stale data *)
  Array.iter (fun cell -> Atomic.set cell None) seg;
  let new_array = {
    log_size       = log_size;
    segment        = seg;
    prev           = Atomic.make (Some src);
    low_water_mark = Atomic.make bottom;
  } in
  for i = top to bottom - 1 do
    put_item new_array i (get_item src i)
  done;
  new_array