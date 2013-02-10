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

module type QueueSig = sig
  type q
  type elem = unit -> unit
  val create : unit -> q
  val push : elem -> q -> unit
  val take : q -> elem
  val length : q -> int
end

module Make (Queue : QueueSig) : EventsSig.S = struct
  include EventsImpl

  let q = Queue.create ()

  let push ch x =
    Queue.push (fun () -> push ch x) q

  let make () =
    let ch = new_channel () in
    events ch, push ch

  let run () =
    let _ =
      (Queue.take q) ()
    in
    Queue.length q

  let run_all () =
    while run () > 0 do () done

end
