(**
   遅延リスト
   @author IT Planning Inc.
   @version $Id$
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

let (!$) x = Lazy.force x

type 'a seq = Nil | Cons of 'a * 'a t
and +'a t = 'a seq Lazy.t

let empty = lazy Nil

let zero = lazy Nil

let return x = lazy (Cons(x, lazy Nil))

let rec map f l = 
  lazy begin
    match !$l with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (f x, map f xs)
  end

let rec foldl f v l = 
  match !$l with
      Nil -> v
    | Cons(x, xs) -> foldl f (f v x) xs

(*        
let rec foldr f v l = 
    match !$l with
        Nil -> v
      | Cons(x, xs) -> f x (foldr f v xs)
*)

let rec foldr f v l = 
  lazy begin
    match !$l with
      Nil -> !$v
    | Cons(x, xs) -> !$(f x (foldr f v xs))
  end

let rec (++) s1 s2 = 
  lazy begin
    match !$s1 with
      Nil -> !$s2
    | Cons (hd, tl) -> Cons (hd, tl ++ s2)
  end

let append = (++)

let rec concat l = 
  lazy begin
    match !$l with
      Nil -> Nil
    | Cons(x, xs) ->
        !$(x ++ (concat xs))
  end
          
let bind s f = concat (map f s)

let rec filter f l = 
  lazy begin 
    match !$l with
        Nil -> Nil
      | Cons(x, xs) ->
          if f x then 
            Cons(x, filter f xs)
          else
            !$(filter f xs)
  end

let rec filter_map f l = 
  lazy begin 
    match !$l with
        Nil -> Nil
      | Cons(x, xs) ->
          match f x with
              Some v -> Cons(v, filter_map f xs)
            | None -> !$(filter_map f xs)
  end

let guard c = if c then return () else zero

let reverse s =
  let rec rev acc l = 
    match !$l with
        Nil -> acc
      | Cons (hd, tl) -> rev (Cons (hd, lazy acc)) tl
  in
    lazy (rev Nil s)

let rec take n l =
  lazy begin
    match n, !$l with
      | 0, _ -> Nil
      | n, Nil -> Nil
      | n, Cons (x, xs) -> 
          Cons(x, take (n-1) xs)
  end

let rec of_list = function
    [] -> empty
  | hd :: tl -> lazy (Cons (hd, of_list tl))

let to_list l =
  let rec loop s l =
    match !$l with
      Nil -> List.rev s
    | Cons(x, xs) -> 
        loop (x :: s) xs
  in
  loop [] l

let to_revlist l =
  let rec loop s l =
    match !$l with
      Nil -> s
    | Cons(x, xs) -> 
        loop (x :: s) xs
  in
  loop [] l
          
let rec unfold f x =
  lazy begin
    match f x with
        Some (a, b) -> 
          Cons (a, unfold f b)
      | None -> Nil
  end

let rec scan f init l =
  lazy begin
    match !$l with
      | Nil -> Nil
      | Cons (x, xs) -> let n = f init x in Cons (n, scan f n xs)
  end

let rec take_while f l = 
  lazy begin
    match !$l with
        Nil -> Nil
      | Cons(x, xs) ->
          if f x then 
            Cons(x, take_while f xs)
          else
            Nil
  end

let rec drop_while f l =
  lazy begin
    match !$l with
        Nil -> Nil
      | Cons(x, xs) as l -> 
          if f x then 
            !$(drop_while f xs)
          else 
            l
  end

let rec zip x y =
  lazy begin
    match !$x with
        Nil -> Nil
      | Cons(x, xs) ->
          match !$y with
              Nil -> Nil
            | Cons(y, ys) ->
                Cons((x,y), zip xs ys)
  end

let rec of_stream stream =
  lazy begin
    try
      Cons (Stream.next stream, of_stream stream)
    with
      Stream.Failure -> Nil
  end

let hd = function | lazy Nil -> failwith "hd" | lazy (Cons (x, xs)) -> x
let tl = function | lazy Nil -> failwith "tl" | lazy (Cons (x, xs)) -> xs
let cons x xs = lazy (Cons(x, xs))

let is_empty = function | lazy Nil -> true | _ -> false

let rec iter f l =
  match !$l with
      Nil -> ()
    | Cons(x, xs) -> 
        f x;
        iter f xs
      
let rec rev l =
  let rec loop s l =
    match !$l with
      Nil -> s
    | Cons(x, xs) ->
        loop (lazy (Cons(x, s))) xs
  in
  loop empty l

  
module OP = struct
  let (++) = append
end
