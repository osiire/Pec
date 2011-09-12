
type 'a event

(** [make ()] makes a new event and sender function.*)
val make : unit -> 'a event * ('a -> unit)
val map : ('a -> 'b) -> 'a event -> 'b event

(** [choose l] is a event which will be raised when one of specified events occurred. *)
val choose : 'a event list -> 'a event
val never : 'a event
(** [join ee] is a event which will be raised when a inner event occurred. 
    "Inner event" is a event taken from outer event [ee].*)
val join : 'a event event -> 'a event
(** [bind e f] is [join (map f e)] *)
val bind : 'a event -> ('a -> 'b event) -> 'b event
val scan : ('a -> 'b -> 'a) -> 'a -> 'b event -> 'a event
val filter : ('a -> bool) -> 'a event -> 'a event
val filter_map : ('a -> 'b option) -> 'a event -> 'b event
val zip : 'a event -> 'b event -> ('a * 'b) event
val take_while : ('a -> bool) -> 'a event -> 'a event
val take_n : int -> 'a event -> 'a event
val once : 'a event -> 'a event

(** [subscribe f e] attaches the [f] to the specified event. 
    The [f] will be called when the [e] will occurred. *)
val subscribe : ('a -> unit) -> 'a event -> unit

(** [run ()] runs PEC event system and returns a number of queuing size of sended data. *)
val run : unit -> int
