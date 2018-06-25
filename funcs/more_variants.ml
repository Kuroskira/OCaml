type point = float * float
type vector = float list
type matrix = float list list

let pi = acos(-. 1.)

let getx : point -> float =
  fun (x,_) -> x
;;

let pt : point = (1.,2.)
let flatpair : float*float = (1.,3.)

let one = getx pt
let one' = getx flatpair

type shape =
  | Point of point
  | Circle of point * float (* Center and radius *)
  | Rect of point * point (* Lower left and upper-right corners *)

let area = function
  | Point _ -> 0.0
  | Circle (_,r) -> Float.pi *. (r ** 2.0)
  | Rect ((x1,y1),(x2,y2)) -> 
    let w = x2 -. x1 in
    let h = y2 -. y1 in
      w *. h  
;;

let center = function
  | Point p -> p
  | Circle (p,_) -> p
  | Rect ((x1,y1),(x2,y2)) ->
    ((x2 -. x1) /. 2.0,
      (y2 -. y1) /. 2.0)
;;

type string_or_int =
  | String of string
  | Int of int
;;

type string_or_int_list = string_or_int list

let rec sum : string_or_int_list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t -> i + sum t
;;

let three = sum [String "2"; Int 1];;

type t = Left of int | Right of int;;
let x = Left 1
let double_right = function
  | Left i -> i
  | Right i -> 2*i
;;

(* Variant type to construct something similar to a list *)
type intlist = Nil | Cons of int * intlist

let lst3 = Cons (3, Nil) (* Similar to 3::[] or [3] *)
let lst123 = Cons(1, Cons(2, lst3)) (* Similar to [1;2;3] *)

let rec sum (l:instlist) : int =
  match l with
  | Nil -> 0
  | Cons(h,t) -> h + sum t
;;

let rec lenght : instlist =
  | Nil -> 0
  | Cons(_,t) -> 1 + lenght t
;;

let empty : intlist =
  | Nil -> true
  | Cons _ -> false
;;

(* Parameterized variants *)
type 'a mylist = Nil | Cons of 'a * 'a mylist;;

let list3 = Cons (3, Nil) (* Similar to [3] *)
let lst_hi = Cons ("hi", Nil) (* Similar to ["hi"] *)

let rec length' : 'a mylist -> int = function
  | Nil -> 0
  | Cons (_,t) -> 1 + length' t
;;

let empty : 'a mylist -> bool = function
  | Nil -> true
  | Cons _ -> false
;;