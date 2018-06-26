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
;;