type point = float * float;;
type vector = float list;;
type matrix = float list list;;

(* let pi = acos(-. 1.) *)

let getx : point -> float =
  fun (x,_) -> x
;;

let pt : point = (1.,2.);;
let flatpair : float*float = (1.,3.);;

let one = getx pt;;
let one' = getx flatpair;;

type shape =
  | Point of point
  | Circle of point * float (* Center and radius *)
  | Rect of point * point (* Lower left and upper-right corners *)
;;

let area = function
  | Point _      -> 0.0
  | Circle (_,r) -> Float.pi *. (r ** 2.0)
  | Rect ((x1,y1),(x2,y2)) -> 
    let w = x2 -. x1 in
    let h = y2 -. y1 in
      w *. h  
;;

let center = function
  | Point   p    -> p
  | Circle (p,_) -> p
  | Rect   ((x1,y1),(x2,y2)) ->
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
  | (Int i)::t    -> i + sum t
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

let rec sum (l:intlist) : int =
  match l with
    | Nil -> 0
    | Cons(h,t) -> h + sum t
;;

let rec length : intlist -> int = function
  | Nil -> 0
  | Cons (_,t) -> 1 + length t
;;

let empty : intlist -> bool = function
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

(* type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree
;; *)

type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  value: 'a;
  left:  'a tree;
  right: 'a tree
}

(* represents
      2
     / \ 
    1   3  *)
let t =
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  };;

(* [mem x t] returns [true] if and only if [x] is a value at some
 * node in tree [t]. 
 *)
let rec mem x = function
  | Leaf -> false
  | Node {value;left;right} -> value = x || mem x left || mem x right
;;

(* a function that computes the preorder traversal of a tree, 
 * in which each node is visited before any of 
 * its children, by constructing a list in which the values occur 
 * in the order in which they would be visited 
 *)
let preorder_lin t =
  let rec pre_acc acc = function
    | Leaf -> acc
    | Node {value;left;right} -> value :: (pre_acc (pre_acc acc right) left)
  in pre_acc [] t
;;

(* NATURAL NUMBERS REPRESENTATION *)
type nat = Zero | Succ of nat;;

let zero = Zero;;
let one = Succ zero;;
let two = Succ one;;
let three = Succ two;;
let four = Succ three;;

let iszero (n:nat) : bool = 
  match n with
    | Zero -> true
    | Succ m -> false
;;

let pred (n:nat) : nat =
  match n with
    | Zero -> failwith "pred Zero is undefined"
    | Succ m -> m
;;

(* Function to add two nat numbers *)
let rec add (n1:nat) (n2:nat) : nat =
  match n1 with
    | Zero -> n2
    | Succ n_minus_1 -> add n_minus_1 (Succ n2)
;;

(* Convert nat into int and vice-versa *)
let rec int_of_nat (n:nat) : int =
  match n with
    | Zero -> 0
    | Succ m -> 1 + int_of_nat m
;;

let rec nat_of_int (i:int) : nat =
  if i < 0 then failwith "nat_of_int is undefined on negative ints"
  else if i = 0 then Zero
  else Succ (nat_of_int (i-1))
;;

(* Determine if a nat number is even or odd usind mutual recursion *)
let rec
  even (n:nat) : bool =
    match n with
      | Zero -> true
      | Succ m -> odd m

and
  odd (n:nat) : bool =
    match n with
      | Zero -> false
      | Succ m -> even m
;;