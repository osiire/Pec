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

module Make : functor (E : EventSig.S) -> sig

  type 'a t

  val return : 'a -> 'a t
  val make : 'a -> 'a E.t -> 'a t
  val event : 'a t -> 'a E.t
  val read : 'a t ->  'a
    
  (** [put s v] puts [v] into [s]. *)
  val put : 'a t -> 'a -> unit

  (** [switch s1 s2] connect the dependency of [s2] into [s1]. signals depends on [s1] still *)
  val switch : 'a t -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val sbind : 'a t -> ('a -> 'b t) -> 'b t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b E.t -> 'a t
  val reduce : 'a -> ('a -> 'a) E.t-> 'a t
  val zip : 'a t -> 'b t -> ('a * 'b) t
  val sequence : 'a t list -> 'a list t
  val fix : ('a t -> 'a t) -> 'a -> 'a t
  val filter : ('a -> bool) -> 'a -> 'a t -> 'a t

  module OP : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (!!) : 'a t -> 'a              (* read *)
    val (<=<) : 'a t -> 'a t -> unit   (* switch *)
    val (<==) : 'a t -> 'a -> unit     (* put *)
  end
end

