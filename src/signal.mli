
type ('a, 'b) t
type s
type e
type 'a signal = ('a, s) t
type 'a event = ('a, e) t
type 'a ievent

val make : (unit -> unit) Queue.t -> 'a event
val return : (unit -> unit) Queue.t -> 'a -> 'a signal
val ievent : ('a, 'b) t -> 'a ievent
val switch : ('a, 'b) t -> 'a ievent -> unit
val listen : ('a -> unit) -> ('a, 'b) t -> unit
val read : 'a signal ->  'a
val put : (unit -> unit) Queue.t -> ('a, 'b) t -> 'a -> unit
val run : (unit -> unit) Queue.t -> int

module OP : sig
  val (!!) : 'a signal -> 'a
  val (<<=) : ('a, 'b) t -> 'a ievent -> unit
  val (<==) : (unit -> unit) Queue.t -> ('a, 'b) t -> 'a -> unit
end

val map : ('a -> 'b) -> 'a ievent -> 'b ievent
val choose : 'a ievent list -> 'a ievent
val never : 'a ievent
val join : 'a ievent ievent -> 'a ievent
val bind : 'a ievent -> ('a -> 'b ievent) -> 'b ievent
val scan : ('a -> 'b -> 'a) -> 'a -> 'b ievent -> 'a ievent
val filter : ('a -> bool) -> 'a ievent -> 'a ievent
val filter_map : ('a -> 'b option) -> 'a ievent -> 'b ievent
val zip : 'a ievent -> 'b ievent -> ('a * 'b) ievent
val take_while : ('a -> bool) -> 'a ievent -> 'a ievent
val take_while_in : ('a -> bool) -> 'a ievent -> 'a ievent
val take_n : int -> 'a ievent -> 'a ievent
val once : 'a ievent -> 'a ievent
val drop_while : ('a -> bool) -> 'a ievent -> 'a ievent
val drop_n : int -> 'a ievent -> 'a ievent
val delay : int -> 'a ievent -> 'a ievent
val pairwise : 'a ievent -> ('a * 'a) ievent

