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

module type Q = sig
  type q
  type elem = unit -> unit
  val queue : q
  val push : elem -> q -> unit
  val take : q -> elem
  val length : q -> int
end

module Default = struct
  type q = (unit -> unit) Queue.t
  type elem = unit -> unit
  let queue : q = Queue.create ()
  include Queue
end

module SyncQueue = struct
  let rec forever f x = 
    let v = f x in forever f v

  let spawn_loop f x =
    ignore (Thread.create (fun () -> forever f x) ())

  type elem = unit -> unit
  type q = {
    queue : elem Queue.t;
    push_ch : elem Event.channel;
    take_ch : elem Event.channel;
  }

  let queue : q = {
    queue = Queue.create ();
    push_ch = Event.new_channel ();
    take_ch = Event.new_channel ();
  }

  let _ =
    let open Event in
    let q = queue in
    spawn_loop (fun () ->
      let ge =
        guard (fun () -> try send q.take_ch (Queue.peek q.queue) with _ -> choose [])
      in
      select [
        wrap (receive q.push_ch) (fun elem -> Queue.push elem q.queue);
        wrap ge (fun () -> ignore (Queue.take q.queue));
      ]) ()

  let push elem q =
    Event.sync (Event.send q.push_ch elem)
    
  let take q =
    Event.sync (Event.receive q.take_ch)

  let length q =
    Queue.length q.queue

end
