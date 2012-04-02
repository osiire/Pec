
type 'a t

val make : Event.queue -> 'a -> 'a t
val return : 'a -> 'a t
val event : 'a t -> 'a Event.t
val read : 'a t ->  'a
val switch : 'a t -> 'a Event.t -> unit
val put : Event.queue -> 'a t -> 'a -> unit

module OP : sig
  val (!!) : 'a t -> 'a
  val (<<=) : 'a t -> 'a Event.t -> unit
  val (<==) : Event.queue -> 'a t -> 'a -> unit
end

