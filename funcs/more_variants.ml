type point = float * float;;
type vector = float list;;
type matrix = float list list;;

(* let pi = acos(-. 1.) *)

let getx : point -> float =
  fun (x,_) -> x
;;

let pt : point = (1.,2.);;
let flatpair : float*float = (1.,3.);;

let one = getx pt;;
let one' = getx flatpair;;

type shape =
  | Point of point
  | Circle of point * float (* Center and radius *)
  | Rect of point * point (* Lower left and upper-right corners *)
;;

let area = function
  | Point _      -> 0.0
  | Circle (_,r) -> Float.pi *. (r ** 2.0)
  | Rect ((x1,y1),(x2,y2)) -> 
    let w = x2 -. x1 in
    let h = y2 -. y1 in
      w *. h  
;;

let center = function
  | Point   p    -> p
  | Circle (p,_) -> p
  | Rect   ((x1,y1),(x2,y2)) ->
    ((x2 -. x1) /. 2.0,
      (y2 -. y1) /. 2.0)
;;

type string_or_int =
  | String of string
  | Int of int
;;

type string_or_int_list = string_or_int list

let rec sum : string_or_int_list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t    -> i + sum t
;;

let three = sum [String "2"; Int 1];;