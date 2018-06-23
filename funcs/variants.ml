type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;

let d:day = Tue;;

let int_of_day d =
  match d with
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 6
  | Fri -> 7
  | Sat -> 8
;;

(* Pokemon types *)
type ptype = 
  TNormal | TFire | TWater

type peff =
  ENormal | ENotVery | ESuper

(* Records: similiar to C Structs *)
(* Pokemon Record *)
type mon = {name: string; hp: int; ptype: ptype}

let c = {name = "Charmender"; hp = 39; ptype = TFire};;

