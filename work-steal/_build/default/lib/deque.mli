type 'a result = Empty | Abort | Value of 'a

type pool_mode = GC | Pool

type shrink_policy = No_shrink | Simple | No_copy | Multi

(** The type of the deque *)
type 'a t

(** Creates a deque with size = 2 ^ size *)
val create : int -> 'a t

(** Pushes an item to the bottom end of the deque *)
val push_bottom : 'a t -> 'a -> pool_mode:pool_mode -> unit

(** Pops an item from the bottom end of the deque *)
val pop_bottom : ?shrink:shrink_policy -> ?pool_mode:pool_mode -> 'a t -> 'a result

(** Steals an item from the top end of *** from another deque *** *)
val steal : 'a t -> 'a result

(** Benchmarking accessors *)
val grows   : 'a t -> int Atomic.t
val shrinks : 'a t -> int Atomic.t
val max_len : 'a t -> int Atomic.t
val min_len : 'a t -> int Atomic.t