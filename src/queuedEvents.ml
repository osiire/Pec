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
  type elem = unit -> unit

  type q = {
      queue : elem Queue.t; 
      lock : Mutex.t; 
      non_empty : Condition.t 
    }
        
  let create () =
    {
     queue = Queue.create ();
     lock = Mutex.create ();
     non_empty = Condition.create ();
    }
      
  let queue : q = create ()

  let push e q =
    Mutex.lock q.lock;
    if Queue.length q.queue = 0 then Condition.signal q.non_empty;
    Queue.add e q.queue;
    Mutex.unlock q.lock
      
  let take q =
    Mutex.lock q.lock;
    while Queue.length q.queue = 0 
    do 
      Condition.wait q.non_empty q.lock 
    done;  
    let x = Queue.take q.queue in
    Mutex.unlock q.lock; x

  let length q =
    Mutex.lock q.lock;
    let x = Queue.length q.queue in
    Mutex.unlock q.lock; x

end

module Make (Q : Q) = struct
  include Events

  let run () =
    let _ =
      (Q.take Q.queue) ()
    in
    Q.length Q.queue

  let run_all () =
    while run () > 0 do () done

end
