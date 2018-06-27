type suit = Clubs | Diamonds | Hearts | Spades;;

type rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King;;

type card = {
  suit: suit;
  rank: rank
};;

let ace_of_clubs = {
  suit = Clubs;
  rank = Ace
};;

let queen_of_hearts = {
  suit = Hearts;
  rank = Queen
};;

let two_of_diamonds = {
  suit = Diamonds;
  rank = Two
};;

let seven_of_spades = {
  suit = Spades;
  rank = Seven
};;

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let negate x =
  -x
;;

let sign (x:int) : sign =
  if x = 0 then Zero
  else if x > 0 then Pos
  else Neg
;;

let quadrant : int*int -> quad option = fun (x,y) ->
  match (sign x, sign y) with
    | (Pos,Pos) -> Some I
    | (Neg,Pos) -> Some II
    | (Neg,Neg) -> Some III
    | (Pos,Neg) -> Some IV
    | (Zero,_) -> None
    | (_,Zero) -> None
;;

let quadrant_when : int*int -> quad option = function
  | (n,m) when n > 0 && m > 0 -> Some I
  | (n,m) when n < 0 && m > 0 -> Some II
  | (n,m) when n < 0 && m < 0 -> Some III
  | (n,m) when n > 0 && m < 0 -> Some IV
  | (n,m) -> None
  
(* BInary Trees *)
type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree
;;

(* the code below constructs this tree:
         4
       /   \
      2     5
     / \   / \
    1   3 6   7 
*)
let t =
  Node (4, 
    Node (2, 
      Node (1, Leaf, Leaf),
      Node (3, Leaf, Leaf)
    ),
    Node (5,
      Node (6, Leaf, Leaf),
      Node (7, Leaf, Leaf)
    )
  );;

let t2 =
  Node (4, 
    Node (2, 
      Node (1, Leaf, Leaf),
      Node (3, Leaf, Leaf)
    ),
    Node (5,
      Node (6, Leaf, Leaf),
      Node (7, Leaf, Leaf)
    )
  );;

let rec size = function
  | Leaf -> 0
  | Node (_,l,r) -> 1 + size l + size r
;;

let rec depth = function
  | Leaf -> 0
  | Node (_,l,r) -> max (1 + depth l) (1 + depth r)
;;

let rec same_shape t1 t2 =
  match (t1,t2) with
    | (Leaf,Leaf) -> true
    | (Node (_,l,r), (Node (_,n,m))) -> same_shape l r && same_shape n m
    | (_, _) -> false
;;

(* Exceptions *)
let rec list_max = function
    | [] -> raise (Failure "list_max")
    | [x] -> x
    | h::t -> max h (list_max t)
;;

let rec list_max_string = function
  | [] -> "empty"
  | [x] -> string_of_int x
  | h::t -> string_of_int (max h (int_of_string (list_max_string t)))
;;