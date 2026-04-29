val time     : (unit -> 'a) -> 'a * float
val mean     : float array -> float
val std_dev  : float array -> float
type gc_stats = {
  minor_collections : int;
  major_collections : int;
  minor_words       : float;
  major_words       : float;
}

val get_gc_diff : (unit -> 'a) -> 'a * gc_stats

val print_header : unit -> unit









val print_row :
  string -> string -> string -> string -> int -> int -> int -> string ->
  float -> float -> float -> float ->
  int -> int -> int -> int -> float -> float -> unit

val run_sections :
  runs:int ->
  seq_time:float ->
  worker_counts:int list ->
  thresholds:int list ->
  steal_policies:Scheduler.steal_policy list ->
  shrink_policies:Deque.shrink_policy list ->
  pool_modes:Deque.pool_mode list ->
  starting_array_sizes:int list ->
  run_ws:(num_workers:int ->
          steal_policy:Scheduler.steal_policy ->
          shrink_policy:Deque.shrink_policy ->
          pool_mode:Deque.pool_mode ->
          threshold:int ->
          starting_array_size:int ->
          Scheduler.stats * float * gc_stats) ->
  run_naive:(num_workers:int -> float) ->
  run_predistributed_balanced:(num_workers:int -> float) ->
  unit
