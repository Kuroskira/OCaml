let double x = 2 * x
let square x = x * x

(* let quad x = double (double x)
let fourth x = square (square x) *)

(* twice : ('a -> 'a) -> 'a -> 'a *)
let twice f x = f (f x)

let quad x = twice double x
let fourth x = twice square x

(* Apply. We can write a function that applies its first input to its second input *)
let apply f x = f x

(* Pipeline. The pipeline operator, which we've previously seen, is a higher-order function *)
let pipeline x f = f x
let (|>) = pipeline
let x = 5 |> double (* 10 *)

(* Compose. We can write a function that composes two other functions *)
let compose f g x = f (g x)

let square_then_double = compose double square
let x = square_then_double 1 (* 2 *)
let y = square_then_double 2 (* 8 *)

(* Both. We can write a function that applies two functions to the same argument and returns a pair of the result *)
let both f g x = (f x, g x)
let ds = both double square
let p = ds 3 (* (6,9) *)

(* Cond. We can write a function that conditionally chooses which of two functions to apply based on a predicate *)
let cond p f g x = 
  if p x then f x else g x


(* MAP *)
(* [map f [x1; x2; ...; xn]] is [f x1; f x2; ...; f xn] *)
let rec map f = function
  | [] -> []
  | h::t -> (f h)::(map f t)

let add1 = map (fun x -> x + 1)
let concat3110 = map (fun x -> x ^ "3110")

(* FILTER *)
(* [filter p l] is the list of elements of [l] that satisfy the predicate [p]. 
 * The order of the elements in the input list is preserved. *)
let rec filter f = function
  | [] -> []
  | h::t -> if f h then h::(filter f t) else filter f t

let even n =
  n mod 2 = 0

let odd n =
  n mod 2 <> 0

let evens = filter even
let odds = filter odd

let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
    in from i j []


(* FOLD RIGHT *)
(* The combine function is the basis for fold_right, it abstracts the commons patterns
   operator and initial value *)
let rec combine op init = function
  | [] -> init 
  | h::t -> op h (combine op init t)

let sum'    = combine (+) 0
let concat' = combine (^) ""

let rec fold_right op lst init = match lst with
  | [] -> init
  | h::t -> op h (fold_right op t init)

let sum    lst = fold_right (+) lst 0
let concat lst = fold_right (^) lst ""

(* FOLD LEFT *)
let rec fold_left op acc = function
  | [] -> acc
  | h::t -> fold_left op (op acc h) t

let sum_l    = fold_left (+) 0
let concat_l = fold_left (^) ""

(* Tail recursive implementation of fold_right, using fold left *)
let fold_right_tr f l accu =
  List.fold_left (fun acc elt -> f elt acc) accu (List.rev l)

(* Folding is so powerful that we can write many other list functions in terms of fold_left or fold_right! *)
let length l = List.fold_left (fun a _ -> a+1) 0 l
let rev l = List.fold_left (fun a x -> x::a) [] l
let map f l = List.fold_right (fun x a -> (f x)::a) l []
let filter f l = List.fold_right (fun x a -> if f x then x::a else a) l []

type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree 

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec foldlist init op = function
  | Nil -> init
  | Cons (h,t) -> op h (foldlist init op t)

(* Fold function on trees *)
let rec foldtree init op = function
  | Leaf -> init
  | Node (v,l,r) -> op v (foldtree init op l) (foldtree init op r)

let size t = foldtree 0 (fun _ l r -> 1 + l + r) t
let depth t = foldtree 0 (fun _ l r -> 1 + max l r) t
let preorder t = foldtree [] (fun x l r -> [x] @ l @ r) t