let rec sum_inner lst a =
  match lst with
  | [] -> a
  | x::xs -> sum_inner xs (x + a)
;;

let sum lst = 
  sum_inner lst 0
;;

let dot_product v1 v2 =
  let (x,y,z) = v1 in
    let (a,b,c) = v2 in
      x*a + y*b + z*c
;;

let a = fun x -> 
          fun y -> 
            fun z -> 
              x + y + z
;;

let rec take_inner n lst a =
  match n with
  | 0 -> a
  | n -> match lst with
         | [] -> a
         | x::xs -> take_inner (n-1) xs (x::a)
;;

let rec rev_inner lst a =
  match lst with
  | [] -> a
  | x::xs -> rev_inner xs (x::a)
;;

(* Tail recursive reverse *)
let rev lst =
  rev_inner lst []
;;

(* Tail recursive take *)
let take n lst = 
  rev (take_inner n lst [])
;;