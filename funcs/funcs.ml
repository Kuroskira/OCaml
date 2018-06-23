let rec power x n =
  match n with
    0 -> 1
  | _ -> x * power x (n-1);;

let rec sumints x =
  match x with
    0 -> 0
  | _ -> x + sumints (x-1);;

let isvowel c =
  match c with
    'a' | 'e' | 'i' | 'o'| 'u' -> true
  | _ -> false;;

Format.printf "%b\n" (isvowel 'a');;

let rec count_true l =
  match l with
    [] -> 0
  | true::t -> 1 + count_true t
  | false::t -> count_true t
;;

let mk_palindrome l =
  l @ List.rev l
;;

let is_palindrome l =
  l = List.rev l
;;

let rec drop_last l =
  match l with
    [] -> []
  | [_] -> []
  | h::t -> h :: drop_last t
;;

let drop' l =
  match l with
    [] -> []
  | list -> List.take list (List.length l - 1)
;;

let rec member x xs =
  match xs with
    [] -> false
  | h::t -> x = h || member x t
;;

let rec make_set l =
  match l with
    [] -> []
  | h::t -> if member h t then make_set t else h :: make_set t
;;

let rec rev_inner a l =
  match l with
    [] -> a
  | h::t -> rev_inner (h :: a) t
;;

let rev l =
  rev_inner [] l
;;

let head = function
  | [] -> 0
  | x::_ -> x
;;

let rec nth_inner n lst a =
  match n with
  | 0 -> head a
  | n -> match lst with
         | [] -> n
         | x::xs -> nth (n-1) xs (x::a)
;;

let nth n lst = nth_inner n lst []
;;

