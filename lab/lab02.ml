let inc x = x + 1
;;

(* require: n >= 0 *)
let rec fact n = if n = 0 then 1 else  n * fact (n-1)
;;

let rec fib n =
  match n with
  | 1 -> 1
  | 2 -> 2
  | n -> fib (n-1) + fib (n-2)
;;

let rec h n pp p =
  match n with
  | 1 -> p
  | n -> h (n-1) p (pp+p)
;;

let fib_fast n = h n 0 1
;;

let id x = x
;;

(* Named arguments plus types declarations *)
let divide ~numerator:(arg1:float) ~denominator:(arg2:float) = arg1 /. arg2
;;

let ($$) x y =
  (x +. y) /. 2.
;;