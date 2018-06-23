let rec prod lst =
  match lst with
  | [] -> 1
  | x::xs -> x * prod xs
;;

let rec concat = function
  | [] -> ""
  | x::xs -> x ^ concat xs
;;

let check_first lst =
  match lst with
  | [] -> false
  | "bigred"::xs -> true
  | _::xs -> false
;;

let two_four lst =
  match lst with
  | [] -> false
  | x::y::[] -> true
  | x::y::z::w::[] -> true
  | _::xs -> false
;;

let two_eq lst =
  match lst with
  | [] -> false
  | x::y::_ -> x = y
  | _::xs -> false
;;

(* require: nth is 0 indexed. *)
let fifth_elem lst =
  if (List.length lst) < 5 then 0
  else match List.nth lst 4 with
       | None -> 0
       | Some n -> n
;;

let dsort lst =
  List.rev (List.sort Pervasives.compare lst)
;;