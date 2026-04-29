(* A type that takes care of managing contexts i.e. what gets passed when
   forking and joining tasks across domains. Here the type is abstract;
   the actual definition in the scheduler takes care of defining what
   gets passed to each task by the scheduler *)

type steal_policy =
  | Random
  | RoundRobin

type stats = {
  steal_count  : int;
  task_count   : int;
  steal_ratio  : float;
  grows        : int;
  shrinks      : int;
  max_capacity : int;
  min_capacity : int;
  pool_mode    : Deque.pool_mode;
}

type ctx
type task = Task of (ctx -> unit)

val fork : ctx -> (ctx -> 'a) -> 'a Future.t
val join : ctx -> 'a Future.t -> 'a
val run  :
  num_workers:int ->
  steal_policy:steal_policy ->
  shrink_policy:Deque.shrink_policy ->
  pool_mode:Deque.pool_mode ->
  initial_tasks:task list ->
  starting_array_size:int ->
  stats

