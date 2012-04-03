
module Make : functor (E : EventSig.S) -> sig

  type 'a t

  val make : 'a -> 'a t
  val return : 'a -> 'a t
  val event : 'a t -> 'a E.t
  val read : 'a t ->  'a
  val switch : 'a t -> 'a E.t -> unit
  val put : 'a t -> 'a -> unit
    
  module OP : sig
    val (!!) : 'a t -> 'a
    val (<<=) : 'a t -> 'a E.t -> unit
    val (<==) : 'a t -> 'a -> unit
  end
end

