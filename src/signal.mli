
type ('a, 'b) signal constraint 'b = [<`MUTABLE|`IMMUTABLE]
type 'a msignal = ('a, [`MUTABLE]) signal
type 'a isignal = ('a, [`IMMUTABLE]) signal

val make : Event.queue -> 'a -> 'a msignal
val return : 'a -> 'a isignal
val event : ('a, 'b) signal -> 'a Event.t
val read : ('a, 'b) signal ->  'a
val switch : ('a, 'b) signal -> 'a Event.t -> unit
val put : Event.queue -> 'a msignal -> 'a -> unit

module OP : sig
  val (!!) : ('a, 'b) signal -> 'a
  val (<<=) : ('a, 'b) signal -> 'a Event.t -> unit
  val (<==) : Event.queue -> 'a msignal -> 'a -> unit
end

