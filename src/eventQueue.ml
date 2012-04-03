
module type I = sig
  type q
  val queue : q
end

module type M = sig
  type q
  type elem = unit -> unit
  val create : unit -> q
  val push : elem -> q -> unit
  val pop : q -> elem
  val take : q -> elem
  val length : q -> int
end

module DefaultQueueI = struct
  type q = (unit -> unit) Queue.t
  let queue : q = Queue.create ()
end

module DefaultQueueM = struct
  type elem = unit -> unit
  type q = (unit -> unit) Queue.t
  include Queue
end
