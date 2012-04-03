
type ('a, 'b) choise2 = 
  [`T1 of 'a | `T2 of 'b]
type ('a, 'b, 'c) choise3 =  
  [ ('a,'b) choise2 | `T3 of 'c ]
type ('a, 'b, 'c, 'd) choise4 =  
  [ ('a,'b, 'c) choise3 | `T4 of 'd ]
type ('a, 'b, 'c, 'd, 'e) choise5 =  
  [ ('a,'b, 'c, 'd) choise4 | `T5 of 'e ]
    
module Make :
  functor (M : EventQueue.M) -> 
    functor (I : EventQueue.I with type q = M.q) -> EventSig.S
