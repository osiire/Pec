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

type ('a, 'b) choice2 = 
  [ `T1 of 'a | `T2 of 'b]
type ('a, 'b, 'c) choice3 =  
  [ ('a,'b) choice2 | `T3 of 'c ]
type ('a, 'b, 'c, 'd) choice4 =  
  [ ('a,'b, 'c) choice3 | `T4 of 'd ]
type ('a, 'b, 'c, 'd, 'e) choice5 =  
  [ ('a,'b, 'c, 'd) choice4 | `T5 of 'e ]

val choice1 : 'a -> [>`T1 of 'a]
val choice2 : 'a -> [>`T2 of 'a]
val choice3 : 'a -> [>`T3 of 'a]
val choice4 : 'a -> [>`T4 of 'a]
val choice5 : 'a -> [>`T5 of 'a]
    
module Make :
  functor (M : EventQueue.M) -> 
    functor (I : EventQueue.I with type q = M.q) -> EventSig.S
