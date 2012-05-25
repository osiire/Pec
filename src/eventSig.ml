(*   
   Copyright (c) 2011 IT Planning inc. All Rights Reserved.
 
   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:
   
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module type S = sig
  type 'a t
  type subscribe_id
      
  (** [make ()] makes a new event and sender function.*)
  val make : unit -> 'a t * ('a -> unit)
  val immediate : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t

  (** [choose l] is a event which will be raised when one of specified events occurred. *)
  val choose : 'a t list -> 'a t
  
  val never : 'a t

  (** [switch ee] is a event which will be raised when a inner event occurred. 
    "Inner event" is a event taken from outer event [ee].*)
  val switch : 'a t t -> 'a t

  (** [sbind e f] is [switch (map f e)] *)
  val sbind : 'a t -> ('a -> 'b t) -> 'b t
  val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val zip : 'a t -> 'b t -> ('a * 'b) t
  val sequence : 'a t list -> 'a list t
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
  val subscribe : ('a -> unit) -> 'a t -> subscribe_id
  val unsubscribe : subscribe_id -> 'a t -> unit

  (** [run ()] runs a PEC event and returns number of events remained in queue. *)
  val run : unit -> int

  val run_all : unit -> unit

  module OP : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end

end
