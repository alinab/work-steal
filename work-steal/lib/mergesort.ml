open Deque
open Scheduler

let warmup_runs = 3


let sequential_sort arr lo hi =
  let sub = Array.sub arr lo (hi - lo) in
  Array.sort compare sub;
  Array.blit sub 0 arr lo (hi - lo)

let merge src dst lo mid hi =
  let i = ref lo in
  let j = ref mid in
  let k = ref lo in
  while !i < mid && !j < hi do
    if src.(!i) <= src.(!j) then begin
      dst.(!k) <- src.(!i); incr i
    end else begin
      dst.(!k) <- src.(!j); incr j
    end;
    incr k
  done;
  while !i < mid do
    dst.(!k) <- src.(!i); incr i; incr k
  done;
  while !j < hi do
    dst.(!k) <- src.(!j); incr j; incr k
  done

let rec mergesort_seq src dst lo hi =
  if hi - lo <= 1 then
    dst.(lo) <- src.(lo)
  else begin
    let mid = lo + (hi - lo) / 2 in
    mergesort_seq dst src lo mid;
    mergesort_seq dst src mid hi;
    merge src dst lo mid hi
  end

let is_sorted arr =
  let n = Array.length arr in
  let ok = ref true in
  for i = 0 to n - 2 do
    if arr.(i) > arr.(i + 1) then ok := false
  done;
  !ok

let rec par_mergesort_ws (ctx : Scheduler.ctx) threshold src dst lo hi =
  if hi - lo <= threshold then begin
    Array.blit src lo dst lo (hi - lo);
    sequential_sort dst lo hi
  end else begin
    let mid       = lo + (hi - lo) / 2 in
    let left_fut  = Scheduler.fork ctx
      (fun ctx -> par_mergesort_ws ctx threshold dst src lo mid) in
    let right_fut = Scheduler.fork ctx
      (fun ctx -> par_mergesort_ws ctx threshold dst src mid hi) in
    Scheduler.join ctx left_fut;
    Scheduler.join ctx right_fut;
    merge src dst lo mid hi
  end

let rec par_mergesort_naive (ctx : Naive_scheduler.ctx) threshold src dst lo hi =
  if hi - lo <= threshold then begin
    Array.blit src lo dst lo (hi - lo);
    sequential_sort dst lo hi
  end else begin
    let mid       = lo + (hi - lo) / 2 in
    let left_fut  = Naive_scheduler.fork ctx
      (fun ctx -> par_mergesort_naive ctx threshold dst src lo mid) in
    let right_fut = Naive_scheduler.fork ctx
      (fun ctx -> par_mergesort_naive ctx threshold dst src mid hi) in
    Naive_scheduler.join ctx left_fut;
    Naive_scheduler.join ctx right_fut;
    merge src dst lo mid hi
  end
let par_mergesort_nosteal arr num_workers =
  let n = Array.length arr in
  let chunk_size = (n + num_workers - 1) / num_workers in

  (* Each worker gets its own private copy *)
  let local_copies =
    Array.init num_workers (fun _ -> Array.copy arr)
  in

  (* Spawn workers *)
  let domains =
    Array.init (num_workers - 1) (fun i ->
      let lo = i * chunk_size in
      let hi = min n (lo + chunk_size) in
      Domain.spawn (fun () ->
        if lo < hi then
          mergesort_seq local_copies.(i) arr lo hi
      )
    )
  in

  (* Main thread handles last chunk *)
  let last = num_workers - 1 in
  let lo = last * chunk_size in
  let hi = n in
  if lo < hi then
    mergesort_seq local_copies.(last) arr lo hi;

  (* Wait for all threads *)
  Array.iter Domain.join domains;

  (* Merge phase (sequential, safe) *)
  let tmp = Array.copy arr in
  let step = ref chunk_size in

  while !step < n do
    let lo = ref 0 in
    while !lo < n do
      let mid = min (!lo + !step) n in
      let hi  = min (!lo + 2 * !step) n in
      if mid < hi then begin
        merge arr tmp !lo mid hi;
        Array.blit tmp !lo arr !lo (hi - !lo)
      end;
      lo := !lo + 2 * !step
    done;
    step := !step * 2
  done

(* let par_mergesort_nosteal arr num_workers =
  let n          = Array.length arr in
  let chunk_size = n / num_workers in
  let tmp        = Array.copy arr in

  let domains = Array.init (num_workers - 1) (fun i ->
    let lo = i * chunk_size in
    let hi = lo + chunk_size in
    Domain.spawn (fun () ->
      (* same mergesort, not Array.sort *)
      let t = Array.copy arr in
      mergesort_seq t arr lo hi
    )
  ) in
  let lo = (num_workers - 1) * chunk_size in
  let t = Array.copy arr in
  mergesort_seq t arr lo n;

  Array.iter Domain.join domains;

  (* merge phase unchanged *)
  let step = ref chunk_size in
  while !step < n do
    let lo = ref 0 in
    while !lo < n do
      let mid = min (!lo + !step) n in
      let hi  = min (!lo + (2 * !step)) n in
      if mid < hi then begin
        merge arr tmp !lo mid hi;
        Array.blit tmp !lo arr !lo (hi - !lo)
      end;
      lo := !lo + (2 * !step)
    done;
    step := !step * 2
  done *)

let run_ws ~num_workers ~steal_policy ~shrink_policy ~pool_mode ~threshold ~n ~starting_array_size =
  let arr = Array.init n (fun _ -> Random.int 1_000_000) in
  let tmp = Array.copy arr in
  let result_fut = Future.create () in
  let root = Scheduler.Task (fun ctx ->
    par_mergesort_ws ctx threshold arr tmp 0 n;
    Future.fill result_fut (is_sorted tmp)
  ) in
  let ((stats, elapsed), gc) = Benchmark.get_gc_diff (fun () ->
    Benchmark.time (fun () ->
      Scheduler.run ~num_workers ~steal_policy ~shrink_policy ~pool_mode
                     ~initial_tasks:[root] ~starting_array_size
    )
  ) in
  (match Future.get result_fut with
  | Some sorted -> assert sorted
  | None        -> failwith "future not filled");
  (stats, elapsed, gc)

let run_naive ~num_workers ~threshold ~n =
  let arr = Array.init n (fun _ -> Random.int 1_000_000) in
  let tmp = Array.copy arr in
  let result_fut = Future.create () in
  let root = Naive_scheduler.Task (fun ctx ->
    par_mergesort_naive ctx threshold arr tmp 0 n;
    Future.fill result_fut (is_sorted tmp)
  ) in
  let (_, elapsed) = Benchmark.time (fun () ->
    Naive_scheduler.run ~num_workers ~initial_tasks:[root]
  ) in
  (match Future.get result_fut with
  | Some sorted -> assert sorted
  | None        -> failwith "future not filled");
  elapsed

let run_predistributed_balanced ~num_workers ~threshold ~n =
  let arr = Array.init n (fun _ -> Random.int 1_000_000) in
  let (_, elapsed) = Benchmark.time (fun () ->
    par_mergesort_nosteal arr num_workers;
    assert (is_sorted arr)
  ) in
  elapsed

let run_experiment ~n ~runs =
  let worker_counts = [1; 2; 4; 8; 16] in
  let thresholds    = [2; 500; 2000; 5000] in
  let steal_policies      = [Scheduler.Random; Scheduler.RoundRobin] in
  (* let shrink_policies     = [Scheduler.No_shrink; Scheduler.Simple; Scheduler.No_copy; Scheduler.Multi] in *)
  let shrink_policies     = [Simple] in
  let starting_array_sizes = [0; 3; 18] in
  let pool_modes = [GC; Pool ] in

  (* sequential baseline *)
  Printf.printf "Computing sequential baseline for mergesort n=%d...\n%!" n;
  for _ = 1 to warmup_runs do
    let arr = Array.init n (fun _ -> Random.int 1_000_000) in
    mergesort_seq arr (Array.copy arr) 0 n
  done;
  let seq_times = Array.init runs (fun _ ->
    let arr = Array.init n (fun _ -> Random.int 1_000_000) in
    let (_, t) = Benchmark.time (fun () ->  mergesort_seq arr (Array.copy arr) 0 n) in t
  ) in
  let seq_time = Benchmark.mean seq_times in
  let seq_sd   = Benchmark.std_dev seq_times in
  Printf.printf "sequential baseline: %.4fs +/- %.4fs\n\n%!" seq_time seq_sd;

  (* single warmup pass *)
  Printf.printf "Warming up...\n%!";
  for _ = 1 to warmup_runs do
    ignore (run_ws ~num_workers:1 ~steal_policy:Random ~shrink_policy:No_shrink ~pool_mode:GC
              ~threshold:1000 ~n ~starting_array_size:0);
    ignore (run_naive ~num_workers:1 ~threshold:1000 ~n);
    ignore (run_predistributed_balanced ~num_workers:1 ~threshold:1000 ~n)
  done;
  Printf.printf "Warmup done.\n\n%!";

  Benchmark.run_sections
    ~runs
    ~seq_time
    ~worker_counts
    ~thresholds
    ~steal_policies
    ~shrink_policies
    ~pool_modes
    ~starting_array_sizes
    ~run_ws:(fun ~num_workers ~steal_policy ~shrink_policy ~pool_mode ~threshold ~starting_array_size ->
      run_ws ~num_workers ~steal_policy ~shrink_policy ~pool_mode ~threshold ~n ~starting_array_size)
    ~run_naive:(fun ~num_workers ->
      run_naive ~num_workers ~threshold:1000 ~n)
    ~run_predistributed_balanced:(fun ~num_workers ->
      run_predistributed_balanced ~num_workers ~threshold:1000 ~n)


let run_experiment_shrinks ~n ~runs =
  (* let worker_counts = [1; 2; 4; 8; 16] in
  let thresholds    = [2; 500; 2000; 5000] in
  let steal_policies      = [Random;RoundRobin] in
  let shrink_policies     = [No_shrink; Simple; No_copy; Multi] in
  let starting_array_sizes = [0; 3; 18] in
  let pool_modes = [GC; Pool ] in *)

  (* let worker_counts = [1; 2; 4; 8; 16] in *)
  let worker_counts = [2; 8; 16] in
  let thresholds    = [10] in
  let steal_policies      = [RoundRobin] in
  (* let shrink_policies     = [No_shrink;Simple; No_copy; Multi] in *)
  let shrink_policies     = [No_shrink; No_copy; Multi;Simple] in
  let starting_array_sizes = [0;3;18] in
  let pool_modes = [GC; Pool ] in



  (* sequential baseline *)
  Printf.printf "Computing sequential baseline for mergesort n=%d...\n%!" n;
  for _ = 1 to warmup_runs do
    let arr = Array.init n (fun _ -> Random.int 1_000_000) in
    mergesort_seq arr (Array.copy arr) 0 n
  done;
  let seq_times = Array.init runs (fun _ ->
    let arr = Array.init n (fun _ -> Random.int 1_000_000) in
    let (_, t) = Benchmark.time (fun () ->  mergesort_seq arr (Array.copy arr) 0 n) in t
  ) in
  let seq_time = Benchmark.mean seq_times in
  let seq_sd   = Benchmark.std_dev seq_times in
  Printf.printf "sequential baseline: %.4fs +/- %.4fs\n\n%!" seq_time seq_sd;

  (* single warmup pass *)
  Printf.printf "Warming up...\n%!";
  for _ = 1 to warmup_runs do
    ignore (run_ws ~num_workers:1 ~steal_policy:Random ~shrink_policy:No_shrink 
              ~pool_mode:GC ~threshold:10 ~n ~starting_array_size:0);
    ignore (run_naive ~num_workers:1 ~threshold:10 ~n);
    ignore (run_predistributed_balanced ~num_workers:1 ~threshold:10 ~n)
  done;
  Printf.printf "Warmup done.\n\n%!";

  Benchmark.run_sections
    ~runs
    ~seq_time
    ~worker_counts
    ~thresholds
    ~steal_policies
    ~shrink_policies
    ~pool_modes
    ~starting_array_sizes
    ~run_ws:(fun ~num_workers ~steal_policy ~shrink_policy ~pool_mode ~threshold ~starting_array_size ->
      run_ws ~num_workers ~steal_policy ~shrink_policy  ~pool_mode ~threshold ~n ~starting_array_size)
    ~run_naive:(fun ~num_workers ->
      run_naive ~num_workers ~threshold:10 ~n)
    ~run_predistributed_balanced:(fun ~num_workers ->
      run_predistributed_balanced ~num_workers ~threshold:10 ~n)

let () =
  Random.self_init ();
  (* run_experiment ~n:100_000 ~runs:1; *)
  

  run_experiment_shrinks ~n:1_000_000 ~runs:5;
 
  (* Array.iteri (fun i cell ->
  let count = Atomic.get cell in
  if count > 0 then
    Printf.printf "  size=%-6d (2^%d): %d visits\n" (1 lsl i) i count
) Deque.capacity_hist *)
