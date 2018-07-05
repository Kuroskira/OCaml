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

let sign' x =
  if x = 0 then `Zero
  else if x > 0 then `Pos
  else `Neg
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

let quadrant' = fun (x,y) ->
  match (sign' x, sign' y) with
    | (`Pos,`Pos) -> Some `I
    | (`Neg,`Pos) -> Some `II
    | (`Neg,`Neg) -> Some `III
    | (`Pos,`Neg) -> Some `IV
    | (`Zero,_) -> None
    | (_,`Zero) -> None 

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
    Node (7,
      Node (6, Leaf, Leaf),
      Node (5, Leaf, Leaf)
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

(* Dictionaries with Trees *)
let d = 
  Node((2,"two"), 
    Node((1,"one"),Leaf,Leaf),
    Node((3,"three"),Leaf,Leaf)
  )

let rec lookup k = function
    | Leaf -> None
    | Node ((k',v),l,r) -> if k = k'
                           then Some v
                           else match lookup k l with
                                  | None -> lookup k r
                                  | value -> value
;;

let rec insert k v = function
    | Leaf -> Node ((k,v),Leaf,Leaf)
    | Node ((k',v'),l,r) -> if k = k' 
                            then Node ((k',v),l,r)
                            else if k > k' 
                            then Node ((k',v'),l,insert k v r)
                            else Node ((k',v'),insert k v l,r)
;;

type 'a tstate = Empty | Max of 'a | Min of 'a | Not_Invariant

let rec maxt = function
  | Leaf -> Empty
  | Node (v,Leaf,Leaf) -> Max v
  | Node (v,_,r) -> match maxt r with
                      | Empty -> Max v
                      | Max n -> if v > n then Max v else Max n
  | _ -> Empty
;;

let rec mint = function
  | Leaf -> Empty
  | Node (v,Leaf,Leaf) -> Min v
  | Node (v,l,_) -> match mint l with
                      | Empty -> Min v
                      | Min n -> if v < n then Min v else Min n
  | _ -> Empty
;;

(* TODO: try to implement it as said in the exercise *)
let rec is_bst = function
  | Leaf -> false
  | Node ((k,v),Leaf,Leaf) -> true
  | Node ((k,v),l,r) -> match (maxt l, mint r) with
                          | (Max (n,_), Min (m,_)) -> n < k && m > k && is_bst l && is_bst r
  | _ -> false
