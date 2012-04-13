(**
   不変キュー
   @author IT Planning Inc.
   @version $Id: fqueue.ml 2 2009-12-10 07:22:10Z osiire $
*)
(*   
   Copyright (c) 2007 IT Planning inc. All Rights Reserved.

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

exception Empty

module S = WSeq
open S.OP

type 'a t = int * 'a S.t * int * 'a S.t

(** 空のキューを生成します *)
let empty () = 0, S.empty, 0, S.empty

(** キューが空ならtrue,それ以外はfalseを返します *)
let is_empty (lenf, _, _, _) = lenf = 0

let check (lenf, f, lenr, r as q) =
  if lenr <= lenf then q
  else (lenf + lenr, f ++ S.reverse r, 0, S.empty)

(** push q x はキューqにxを追加します *)
let push (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, S.cons x r)

(** キューの最初の要素を取得します. キューが空の場合はEmpty例外が発生します. *)
let head (_, s, _, _) =
  if S.is_empty s then
    raise Empty
  else
    S.hd s

(** キューの最初の要素を除いた残りのキューを返します. キューが空の場合は、 Empty例外が発生します *) 
let tail (lenf, s, lenr, r) =
  if S.is_empty s then
    raise Empty
  else
    check (lenf - 1, S.tl s, lenr, r)

