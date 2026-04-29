open Scheduler
open Deque


type gc_stats = {
  minor_collections : int;
  major_collections : int;
  minor_words       : float;
  major_words       : float;
}

(* GC Stats*)

let get_gc_diff f =
  Gc.compact ();
  let before = Gc.stat () in
  let result = f () in
  let after  = Gc.stat () in
  let gc = {
    minor_collections = after.Gc.minor_collections - before.Gc.minor_collections;
    major_collections = after.Gc.major_collections - before.Gc.major_collections;
    minor_words       = after.Gc.minor_words -. before.Gc.minor_words;
    major_words       = after.Gc.major_words -. before.Gc.major_words;
  } in
  (result, gc)


let time f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  (result, t1 -. t0)

let mean arr =
  Array.fold_left (+.) 0.0 arr /. float_of_int (Array.length arr)

let std_dev arr =
  let m = mean arr in
  let variance = Array.fold_left (fun acc x ->
    acc +. (x -. m) *. (x -. m)
  ) 0.0 arr /. float_of_int (Array.length arr) in
  sqrt variance

let print_header () =
  Printf.printf "%-10s %-12s %-14s %-12s %-13s %-10s %-22s %-24s %-8s %-8s %-8s %-12s %-8s %-12s %-10s %-10s %-14s %-14s\n"
    "scheduler" "steal_policy" "shrink_policy" "pool_mode" "starting_size" "workers" "threshold"
    "steal_ratio" "avg(s)" "sd(s)" "speedup" "tasks/s"
    "grows" "shrinks" "minor_col" "major_col" "minor_words(M)" "major_words(M)";
  Printf.printf "%s\n" (String.make 200 '-')

let print_row scheduler steal_policy shrink_policy pool_mode starting_size workers threshold
              steal_ratio avg_time sd_time speedup throughput
              grows shrinks avg_minor_col avg_major_col avg_minor_words avg_major_words =
  let tp_str = if throughput > 0.0
               then Printf.sprintf "%.0f" throughput
               else "-"
  in
  let steal_str = Printf.sprintf "%s" steal_ratio in
  Printf.printf "%-10s %-12s %-14s %-12s %-13d %-10d %-22d %-24s %-8.4f %-8.4f %-8.2f %-12s %-8d %-12d %-10d %-10d %-14.2f %-14.2f\n"
    scheduler steal_policy shrink_policy pool_mode starting_size workers threshold steal_str
    avg_time sd_time speedup tp_str
    grows shrinks
    avg_minor_col avg_major_col (avg_minor_words /. 1_000_000.0) (avg_major_words /. 1_000_000.0)

let run_sections ~runs ~seq_time ~worker_counts ~thresholds ~steal_policies ~shrink_policies ~pool_modes ~starting_array_sizes
                 ~run_ws ~run_naive ~run_predistributed_balanced =

  (* work-stealing — one section per policy *)
  List.iter (fun steal_policy ->
    
    List.iter (fun shrink_policy ->
      let steal_policy_str = match steal_policy with
        | Random     -> "random"
        | RoundRobin -> "round-robin"
      in

      let shrink_policy_str = match shrink_policy with
        | No_shrink -> "no_shrink"
        | Simple    -> "simple"
        | No_copy   -> "no_copy"
        | Multi     -> "multi"
      in

     
  
      Printf.printf "\n=== Work-stealing (%s victim) ===\n%!" steal_policy_str;
      print_header ();
      List.iter (fun w ->
        
        List.iter (fun t ->
          List.iter (fun pool_mode -> 
          List.iter (fun s ->
          let ws_times    = Array.make runs 0.0 in
          let ws_steals   = Array.make runs 0 in
          let ws_tasks    = Array.make runs 0 in
          let ws_grows    = Array.make runs 0 in
          let ws_shrinks  = Array.make runs 0 in
          (* let ws_max_cap  = Array.make runs 0 in
          let ws_min_cap  = Array.make runs 0 in *)

          let ws_minor_col  = Array.make runs 0 in
          let ws_major_col  = Array.make runs 0 in
          let ws_minor_words = Array.make runs 0.0 in
          let ws_major_words = Array.make runs 0.0 in

          let pool_mode_str = match pool_mode with
              | GC   -> "gc"
              | Pool -> "pool"
            in
            
          for i = 0 to runs - 1 do
    
            let (stats, elapsed, gc) =
              run_ws ~num_workers:w ~steal_policy:steal_policy ~shrink_policy:shrink_policy ~pool_mode:pool_mode ~threshold:t ~starting_array_size:s in
            ws_times.(i)   <- elapsed;
            ws_steals.(i)  <- stats.steal_count;
            ws_tasks.(i)   <- stats.task_count;
            ws_grows.(i)   <- stats.grows;
            ws_shrinks.(i) <- stats.shrinks;
            (* ws_max_cap.(i) <- stats.max_capacity;
            ws_min_cap.(i) <- stats.min_capacity; *)
            ws_minor_col.(i)  <- gc.minor_collections;
            ws_major_col.(i)  <- gc.major_collections;
            ws_minor_words.(i) <- gc.minor_words;
            ws_major_words.(i) <- gc.major_words;


          done;
          let avg_time   = mean ws_times in
          let sd_time    = std_dev ws_times in
          let tot_steals = Array.fold_left (+) 0 ws_steals in
          let tot_tasks  = Array.fold_left (+) 0 ws_tasks in
          let ratio      = if tot_tasks = 0 then 0.0
                           else float_of_int tot_steals
                                /. float_of_int tot_tasks
          in
          let throughput  = float_of_int tot_tasks /. avg_time in
          let avg_grows   = Array.fold_left (+) 0 ws_grows   / runs in
          let avg_shrinks = Array.fold_left (+) 0 ws_shrinks / runs in
          (* let avg_max_cap = Array.fold_left (+) 0 ws_max_cap / runs in
          let avg_min_cap = Array.fold_left (+) 0 ws_min_cap / runs in *)
          let avg_minor_col   = Array.fold_left (+) 0 ws_minor_col   / runs in
          let avg_major_col   = Array.fold_left (+) 0 ws_major_col   / runs in
          let avg_minor_words = Array.fold_left (+.) 0.0 ws_minor_words /. float_of_int runs in
          let avg_major_words = Array.fold_left (+.) 0.0 ws_major_words /. float_of_int runs in
          
          
            print_row "ws" steal_policy_str shrink_policy_str pool_mode_str s w t
            (Printf.sprintf "%d/%d(%.1f%%)" tot_steals tot_tasks (ratio *. 100.0))
              avg_time sd_time (seq_time /. avg_time) throughput
              avg_grows avg_shrinks avg_minor_col avg_major_col avg_minor_words avg_major_words
            )  starting_array_sizes;

           Printf.printf "%s\n" (String.make 150 '-')) pool_modes;
        )thresholds;
      )worker_counts;
      Printf.printf "%s\n" (String.make 150 '-')
    ) shrink_policies
  ) steal_policies;

  (* naive — one section, no steal policy *)
  Printf.printf "\n=== Naive shared-queue scheduler ===\n%!";
  print_header ();
  List.iter (fun w ->
    Printf.printf "  running naive workers=%d...\n%!" w;
    let naive_times = Array.make runs 0.0 in
    for i = 0 to runs - 1 do
      naive_times.(i) <- run_naive ~num_workers:w
    done;
    let avg_time_n = mean naive_times in
    let sd_time_n  = std_dev naive_times in
    List.iter (fun t ->
      print_row "naive" "-" "-" "-" 0 w t "-"
        avg_time_n sd_time_n (seq_time /. avg_time_n) 0.0
        0 0 0 0 0.0 0.0
    ) thresholds
  ) worker_counts;
  Printf.printf "%s\n" (String.make 100 '-');

  (* pre-distributed balanced task scheduler — one section, no steal policy *)
  Printf.printf "\n=== Pre distributed balanced task scheduler ===\n%!";
  print_header ();
  List.iter (fun w ->
    Printf.printf "  running pre-distributed balanced workers=%d...\n%!" w;
    let predistributed_times = Array.make runs 0.0 in
    for i = 0 to runs - 1 do
      predistributed_times.(i) <- run_predistributed_balanced ~num_workers:w
    done;
    let avg_time_n = mean predistributed_times in
    let sd_time_n  = std_dev predistributed_times in
    List.iter (fun t ->
      print_row "predistributed" "-" "-" "-" 0 w t "-"
        avg_time_n sd_time_n (seq_time /. avg_time_n) 0.0
        0 0 0 0 0.0 0.0
    ) thresholds
  ) worker_counts;
  Printf.printf "%s\n" (String.make 100 '-')


