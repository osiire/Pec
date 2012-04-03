(**
 * A simple implementation of Push style Event Combinator.
 *)

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

(* $Id$ *)

module Make ( E : EventSig.S ) = struct
  type 'a t = {
    mutable value : 'a;
    mutable event : 'a E.t;
    switcher : ('a E.t -> unit);
  }
      
  let event t =
    t.event
      
  let switch t (e : 'a E.t) =
    t.switcher e
      
  let read t =
    t.value
      
  let put t x = 
    let e, sender = E.make () in
    switch t e;
    sender x
      
  let return x =
    {
      value = x;
      event = E.never;
      switcher = (fun _ -> ());
    }
      
  let make x =
    let ee, e_sender = E.make () in
    let e = E.join ee in
    let t = {
      value = x;
      event = e;
      switcher = e_sender;
    } in
    E.subscribe (fun x -> t.value <- x) e;
    t

  let map f t =
    let t' = make (f t.value) in
    switch t' (E.map f t.event);
    t'

  module OP = struct
    let (!!) t = read t
    let (<<=) t e = switch t e
    let (<==) t x = put t x
  end
end
