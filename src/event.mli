
type 'a t
type queue

type ('a, 'b) choise2 = 
    [`T1 of 'a | `T2 of 'b]
type ('a, 'b, 'c) choise3 =  
    [ ('a,'b) choise2 | `T3 of 'c ]
type ('a, 'b, 'c, 'd) choise4 =  
    [ ('a,'b, 'c) choise3 | `T4 of 'd ]
type ('a, 'b, 'c, 'd, 'e) choise5 =  
    [ ('a,'b, 'c, 'd) choise4 | `T5 of 'e ]

(** [make ()] makes a new event and sender function.*)
val make_queue : unit -> queue
val make : queue -> 'a t * ('a -> unit)
val map : ('a -> 'b) -> 'a t -> 'b t
val return : queue -> 'a -> 'a t
(** [choose l] is a event which will be raised when one of specified events occurred. *)
val choose : 'a t list -> 'a t
val never : 'a t
(** [join ee] is a event which will be raised when a inner event occurred. 
    "Inner event" is a event taken from outer event [ee].*)
val join : 'a t t -> 'a t
(** [bind e f] is [join (map f e)] *)
val bind : 'a t -> ('a -> 'b t) -> 'b t
val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t
val take_while : ('a -> bool) -> 'a t -> 'a t
val take_while_in : ('a -> bool) -> 'a t -> 'a t
val take_n : int -> 'a t -> 'a t
val once : 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val drop_n : int -> 'a t -> 'a t
val delay : int -> 'a t -> 'a t
val pairwise : 'a t -> ('a * 'a) t

(** [subscribe f e] attaches the [f] to the specified event. 
    The [f] will be called when the [e] will occurred. *)
val subscribe : ('a -> unit) -> 'a t -> unit

(** [run ()] runs a PEC event and returns number of events remained in queue. *)
val run : queue -> int

module OP : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end
