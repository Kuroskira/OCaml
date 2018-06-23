type student = {name: string ; last_name: string ; gpa: float}

let s = {name = "Patrick" ; last_name = "Persico" ; gpa = 6.0}
;;

let s_name s =
  let {name;last_name;gpa} = s in name
;;

let mk_student name last_name gpa =
  {name ; last_name ; gpa}
;;

type poketype = Normal | Fire | Water | Psyco

type pokemon = {name: string ; hp: int ; ptype: poketype}

let charizard = {name = "Charizard" ; hp = 78 ; ptype = Fire};;

let metapod = {name = "Metapod" ; hp = 50 ; ptype = Normal};;

let mew = {name = "Mew" ; hp = 100 ; ptype = Psyco};;

(* Extract int from an option *)
let extract o =
  match o with
  | Some i -> string_of_int i
  | None -> ""
;;

let rec list_max = function
  | [] -> None
  | h::t -> match list_max t with
            | None -> Some h
            | Some m -> Some (max h m)
;;

let safe_hd = function
  | [] -> None
  | x::_ -> Some x
;;

let safe_tl = function
  | [] -> None
  | _::xs -> Some xs
;;

let pokemons = [charizard;metapod;mew];;

let rec max_hp = function
  | [] -> None
  | x::xs -> match max_hp xs with
             | None -> Some x
             | Some p -> if p.hp > x.hp 
                         then Some p
                         else Some x
;;

(* type months = Jan | Feb | Mar | Jun | Jul | Aug | Sep | Oct | Nov | Dec *)

exception Not_a_date

let is_before d1 d2 =
  let (x,y,z) = d1 in
  let (a,b,c) = d2 in
    if (x > 31 || x <= 0) || (y > 12 || y <= 0) || (z < 1) ||
        (a > 31 || a <= 0) || (b > 12 || b <= 0) || (c < 1)
    then raise Not_a_date
    else if d1 = d2 then false
    else if z < c then true
    else if z = c && y < b then true
    else if z = c && y = b && x < a then true
    else false
;;

let dates = [(9,9,1995);(28,7,1996);(24,3,1995)];;

let rec earliest = function
  | [] -> None
  | x::xs -> match earliest xs with
             | None -> Some x
             | Some d -> if is_before x d 
                         then Some x
                         else Some d
;;

(* Insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d

(* Find the value v to which key k is bound, if any, in the association list *)
let rec lookup k = function
  | [] -> None
  | (k', v)::t -> if k = k' then Some v else lookup k t
;;