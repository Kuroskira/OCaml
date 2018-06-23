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

let last lst =
  List.hd (List.rev lst)
;;

let rec mem n lst = 
  match lst with
  | [] -> false
  | x::xs -> (n = x) || mem n xs
;; 

let any_zeros lst =
  mem 0 lst
;;

let rec take n lst =
  match n with
  | 0 -> []
  | n -> match lst with
         | [] -> []
         | x :: xs -> x :: (take (n-1) xs)

let rec drop n lst =
  match n with
  | 0 -> lst
  | n -> match lst with
         | [] -> []
         | x :: xs -> drop (n-1) xs
;;

let rec take_inner n lst c =
  match n with
  | 0 -> c
  | n -> match lst with
         | [] -> c
         | x::xs -> take_inner (n-1) xs (x::c)
;;

let tail_take n lst = take_inner n lst []
;;

let rec drop_inner n lst c =
  match n with
  | 0 -> c @ lst 
  | n -> match lst with
         | [] -> c
         | x::xs -> drop_inner (n-1) xs c
;;

let tail_drop n lst = drop_inner n lst []
;;

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i > j then l
  else from i (j-1) (j::l)
;;

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *) 
let (--) i j =
  from i j []

let longlist = 0 -- 1_000_000
;;

let rec tsum lst a =
  match lst with
  | [] -> a
  | x::xs -> tsum xs (x+a)
;;

let sum lst = tsum lst 0

let rec rsum = function
  | [] -> 0
  | x::xs -> x + rsum xs;;

exception Empty_list

let head = function
  | [] -> raise Empty_list
  | x::_ -> x
;;

exception Out_of_bounds

let rec nth_inner n lst a =
  match n with
  | 0 -> head a
  | n -> match lst with
         | [] -> raise Out_of_bounds
         | x::xs -> nth_inner (n-1) xs (x::a)
;;

let nth n lst = nth_inner n lst []
;;