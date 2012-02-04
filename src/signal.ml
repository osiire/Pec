(*******************************************************************)
(*                          Amthing                                *)
(*                                                                 *)
(*  Satoshi Ogasawara Copyright 2012 All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library    *)
(*  General Public License, with the special exception on linking  *)
(*  described in file LICENSE.                                     *)
(*******************************************************************)

(* $Id$ *)

module E = Pec

type ('a, 'b) t = {
    value : 'a ref option;
    event : 'a E.event;
    switcher : ('a E.event -> unit);
  }

type s = unit
type e = unit

type 'a signal = ('a, s) t
type 'a event = ('a, e) t
type 'a ievent = 'a E.event

let ievent t =
  t.event

let switch t (e : 'a ievent) =
  t.switcher e

let listen f t =
  E.subscribe f t.event

let read (t : 'a signal) =
  match t.value with
    None -> assert false
  | Some r -> !r

let put queue t x = 
  let e, sender = E.make queue in
  switch t e;
  sender x

let make queue : 'a event =
  let ee, e_sender = E.make queue in
  let event = E.join ee in
  {
    value = None;
    event = event;
    switcher = e_sender;
  }

let return queue x =
  let ee, e_sender = E.make queue in
  let event = E.join ee in
  let t = {
    value = Some (E.value x event);
    event = event;
    switcher = e_sender;
  }
  in
  put queue t x;
  t

let run queue = 
  E.run queue

module OP = struct
  let (!!) t = read t
  let (<<=) t e = switch t e
  let (<==) queue t x = put queue t x
end

module type S = sig
  val map : ('a -> 'b) -> 'a ievent -> 'b ievent
  val choose : 'a ievent list -> 'a ievent
  val never : 'a ievent
  val join : 'a ievent ievent -> 'a ievent
  val bind : 'a ievent -> ('a -> 'b ievent) -> 'b ievent
  val scan : ('a -> 'b -> 'a) -> 'a -> 'b ievent -> 'a ievent
  val filter : ('a -> bool) -> 'a ievent -> 'a ievent
  val filter_map : ('a -> 'b option) -> 'a ievent -> 'b ievent
  val zip : 'a ievent -> 'b ievent -> ('a * 'b) ievent
  val take_while : ('a -> bool) -> 'a ievent -> 'a ievent
  val take_while_in : ('a -> bool) -> 'a ievent -> 'a ievent
  val take_n : int -> 'a ievent -> 'a ievent
  val once : 'a ievent -> 'a ievent
  val drop_while : ('a -> bool) -> 'a ievent -> 'a ievent
  val drop_n : int -> 'a ievent -> 'a ievent
  val delay : int -> 'a ievent -> 'a ievent
  val pairwise : 'a ievent -> ('a * 'a) ievent
end

include (Pec : S)
