
type 'a event
val run : unit -> int
val make : unit -> 'a event * ('a -> unit)
val map : ('a -> 'b) -> 'a event -> 'b event
val choose : 'a event list -> 'a event
val never : 'a event
val join : 'a event event -> 'a event
val bind : 'a event -> ('a -> 'b event) -> 'b event
val scan : ('a -> 'b -> 'a) -> 'a -> 'b event -> 'a event
val filter : ('a -> bool) -> 'a event -> 'a event
val filter_map : ('a -> 'b option) -> 'a event -> 'b event
val zip : 'a event -> 'b event -> ('a * 'b) event
val take_while : ('a -> bool) -> 'a event -> 'a event
val take_n : int -> 'a event -> 'a event
val once : 'a event -> 'a event

val subscribe : ('a -> unit) -> 'a event -> unit
