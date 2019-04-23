let rec fold_left op acc = function
  | [] -> acc
  | h::t -> fold_left op (op acc h) t
;;

let rec fold_right op lst init = match lst with
  | [] -> init
  | h::t -> op h (fold_right op t init)
;;

let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

let ($) f x = f x;;

let (@@) f g x = x |> g |> f

let rec repeat f n x =
  match n with
    | 0 -> x
    | n -> repeat f (n-1) (f x)

let floatprod l = fold_left ( *. ) 1.0 l
let floatprodr l = fold_right ( *. ) l 1.0

let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
    in from i j []

let rec sum_cube_odd n =
  let list = (--) 0 n in
  let odds = List.filter list (fun x -> x mod 3 == 0) in
  let cubed = List.map odds (fun x -> float_of_int x ** 3.) in
  let sum = fold_left (+) 0 (List.map cubed int_of_float) in
  sum
