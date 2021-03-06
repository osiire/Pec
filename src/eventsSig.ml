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
  type 'a channel
  type subscribe_id

  val new_channel : unit -> 'a channel
  val push : 'a channel -> 'a -> unit
  val events : 'a channel -> 'a t

  (** [make ()] makes a new event and sender function.*)
  val make : unit -> 'a t * ('a -> unit)
  val immediate : 'a -> 'a t

  (** [subscribe f e] attaches the [f] to the specified event.
      The [f] will be called when the [e] will occurred. *)
  val subscribe : ('a -> unit) -> 'a t -> subscribe_id
  val unsubscribe : subscribe_id -> 'a t -> unit
  val async_read : ('a -> unit) -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t

  (** [choose l] is a event which will be raised when one of specified events occurred. *)
  val choose : 'a t list -> 'a t
  val choice1 : 'a t -> [> `T1 of 'a] t
  val choice2 : 'a t -> [> `T2 of 'a] t
  val choice3 : 'a t -> [> `T3 of 'a] t
  val choice4 : 'a t -> [> `T4 of 'a] t
  val choice5 : 'a t -> [> `T5 of 'a] t

  val never : 'a t

  (** [switch ee] is a event which will be raised when a inner event occurred.
    "Inner event" is a event taken from outer event [ee].*)
  val switch : 'a t t -> 'a t

  (** [sbind e f] is [switch (map f e)] *)
  val sbind : 'a t -> ('a -> 'b t) -> 'b t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val zip : 'a t -> 'b t -> ('a * 'b) t
  val sequence : 'a t list -> 'a list t
  val take_while : ('a -> bool) -> 'a t -> 'a t
  val take_n : int -> 'a t -> 'a t
  val once : 'a t -> 'a t
  val drop_while : ('a -> bool) -> 'a t -> 'a t
  val drop_n : int -> 'a t -> 'a t
  val delay : int -> 'a t -> 'a t
  val pairwise : 'a t -> ('a * 'a) t

  val split : 'a t -> ('a -> bool) -> 'a t * 'a t
  val split_n : 'a t -> int -> 'a t list

  module type LazyListSig = sig
    type 'a t
    val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b t
  end

  module Import (L : LazyListSig) : sig
    (**
        be careful, the lazy list will block the thread until a new event occured.
        you should evaluate the lazy list outside of event loop.
        (this implementaion is multi-thread safe.)
     *)
    val to_llist : 'a t -> 'a L.t
  end

  type ('a, 'b) choice2 =
      [ `T1 of 'a | `T2 of 'b]
  type ('a, 'b, 'c) choice3 =
      [ ('a,'b) choice2 | `T3 of 'c ]
  type ('a, 'b, 'c, 'd) choice4 =
      [ ('a,'b, 'c) choice3 | `T4 of 'd ]
  type ('a, 'b, 'c, 'd, 'e) choice5 =
      [ ('a,'b, 'c, 'd) choice4 | `T5 of 'e ]

  val run : unit -> int
  val run_all : unit -> unit

(*
  val future : ('a -> 'b) -> 'a -> 'b t
  val timeout : float -> 'a -> 'a t
  val repeat_timeout : float -> 'a -> 'a t
*)

  module OP : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end

end
