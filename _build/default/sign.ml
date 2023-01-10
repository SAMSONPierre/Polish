open Def
open String_is_get_read_eval
(** sign **)

let rec union (l1:'a list) (l2:'a list) =
  let rec f x l = 
  match l with
    | [] -> true
    | hd::tl ->
      if x = hd then false else f x tl
   in
  match l2 with
  | [] -> l1
  | hd::tl ->
    if f hd l1 then
      union (hd::l1) tl
    else
      union l1 tl
;;

let rec intersect l1 l2 =
    match l1 with [] -> []
        | h1::t1 -> (
          match l2 with [] -> []
              | h2::t2 when h1 < h2 -> intersect t1 l2
              | h2::t2 when h1 > h2 -> intersect l1 t2
              | h2::t2 -> (
                match intersect t1 t2 with [] -> [h1]
                    | h3::t3 as l when h3 = h1 -> l
                    | h3::t3 as l -> h1::l
              )
        )
;;

let rec contraire l1 l2 = 
  match l1 with 
  | [] -> []
  | h::q -> if(is_present h l2)then
              contraire q l2
            else
              h::contraire q l2
;;

let string_of_sign(sign:sign) : string = 
  match sign with
  | Pos -> "+"
  | Zero -> "0"
  | Neg -> "-"
  | Error -> "!"
;;
let rec string_of_sign_list(sign:sign list) : string = 
  match sign with
  | h::q -> string_of_sign h ^ string_of_sign_list q
  | [] -> "\n"
;;

let rec print_sign_var(env:(string * sign list) list) : string = 
  match env with
  | (name,sign)::q -> name ^ " : " ^ string_of_sign_list sign ^ print_sign_var q
  | [] -> ""
;;

let sign_of_num(x:int) : sign list =
  if(x>0)then [Pos]
  else if(x<0) then [Neg]
  else [Zero]
;;

let rec size(x:sign list) : int =
  match x with
  | h::q -> 1 + size(q)
  | [] -> 0 
;;
let add_sign(elem:sign)(liste:sign list) : sign list = 
  if(is_present elem liste)then
    liste
  else
    elem::liste
;;

let rec sign_add_aux(x:sign)(y:sign list) : sign list =
  match x with
  | Pos -> 
    (match y with
    | Pos::q -> union([Pos])(sign_add_aux(x)(q))
    | Neg::q -> union([Pos;Zero;Neg])(sign_add_aux(x)(q))
    | Zero::q -> union([Pos])(sign_add_aux(x)(q))
    | Error::q -> union([Error])(sign_add_aux(x)(q))
    | [] -> [])
  | Neg -> 
    (match y with
    | Pos::q -> union([Pos;Zero;Neg])(sign_add_aux(x)(q))
    | Neg::q -> union([Neg])(sign_add_aux(x)(q))
    | Zero::q -> union([Neg])(sign_add_aux(x)(q))
    | Error::q -> union([Error])(sign_add_aux(x)(q))
    | [] -> [])
  | Zero ->
    (match y with
    | Pos::q -> union([Pos])(sign_add_aux(x)(q))
    | Neg::q -> union([Neg])(sign_add_aux(x)(q))
    | Zero::q -> union([Zero])(sign_add_aux(x)(q))
    | Error::q ->  union([Error])(sign_add_aux(x)(q))
    | [] -> [])
  | Error -> 
    (match y with
    | h::q -> union([Error])(sign_add_aux(x)(q))
    | [] -> [])
;;

let rec sign_add(x:sign list)(y:sign list) : sign list = 
  match x with
  | h::q -> union(sign_add_aux h y)(sign_add q y)
  | [] -> []
;;

let rec sign_sub_aux(x:sign)(y:sign list) : sign list =
  match x with
  | Pos -> 
    (match y with
    | Pos::q -> union([Pos;Zero;Neg])(sign_sub_aux(x)(q))
    | Neg::q -> union([Pos])(sign_sub_aux(x)(q))
    | Zero::q -> union([Pos])(sign_sub_aux(x)(q))
    | Error::q -> union([Error])(sign_sub_aux(x)(q))
    | [] -> [])
  | Neg -> 
    (match y with
    | Pos::q -> union([Neg])(sign_sub_aux(x)(q))
    | Neg::q -> union([Pos;Zero;Neg])(sign_sub_aux(x)(q))
    | Zero::q -> union([Neg])(sign_sub_aux(x)(q))
    | Error::q -> union([Error])(sign_sub_aux(x)(q))
    | [] -> [])
  | Zero ->
    (match y with
    | Pos::q -> union([Neg])(sign_sub_aux(x)(q))
    | Neg::q -> union([Pos])(sign_sub_aux(x)(q))
    | Zero::q -> union([Zero])(sign_sub_aux(x)(q))
    | Error::q ->  union([Error])(sign_sub_aux(x)(q))
    | [] -> [])
  | Error -> 
    (match y with
    | h::q -> union([Error])(sign_sub_aux(x)(q))
    | [] -> [])
;;

let rec sign_sub(x:sign list)(y:sign list) : sign list = 
  match x with
  | h::q -> union(sign_sub_aux h y)(sign_sub q y)
  | [] -> []
;;

let rec sign_mul_aux(x:sign)(y:sign list) : sign list =
  match x with
  | Pos -> 
    (match y with
    | Pos::q -> union([Pos])(sign_mul_aux(x)(q))
    | Neg::q -> union([Neg])(sign_mul_aux(x)(q))
    | Zero::q -> union([Zero])(sign_mul_aux(x)(q))
    | Error::q -> union([Error])(sign_mul_aux(x)(q))
    | [] -> [])
  | Neg -> 
    (match y with
    | Pos::q -> union([Neg])(sign_mul_aux(x)(q))
    | Neg::q -> union([Pos])(sign_mul_aux(x)(q))
    | Zero::q -> union([Zero])(sign_mul_aux(x)(q))
    | Error::q -> union([Error])(sign_mul_aux(x)(q))
    | [] -> [])
  | Zero ->
    (match y with
    | Pos::q -> union([Zero])(sign_mul_aux(x)(q))
    | Neg::q -> union([Zero])(sign_mul_aux(x)(q))
    | Zero::q -> union([Zero])(sign_mul_aux(x)(q))
    | Error::q ->  union([Error])(sign_mul_aux(x)(q))
    | [] -> [])
  | Error -> 
    (match y with
    | h::q -> union([Error])(sign_mul_aux(x)(q))
    | [] -> [])
;;

let rec sign_mul(x:sign list)(y:sign list) : sign list = 
  match x with
  | h::q -> union(sign_mul_aux h y)(sign_mul q y)
  | [] -> []
;;

let rec sign_mod_aux(x:sign)(y:sign list) : sign list =
  match x with
  | Pos -> 
    (match y with
    | Pos::q -> union([Pos])(sign_mod_aux(x)(q))
    | Neg::q -> union([Neg])(sign_mod_aux(x)(q))
    | Zero::q -> union([Error])(sign_mod_aux(x)(q))
    | Error::q -> union([Error])(sign_mod_aux(x)(q))
    | [] -> [])
  | Neg -> 
    (match y with
    | Pos::q -> union([Neg])(sign_mod_aux(x)(q))
    | Neg::q -> union([Pos])(sign_mod_aux(x)(q))
    | Zero::q -> union([Error])(sign_mod_aux(x)(q))
    | Error::q -> union([Error])(sign_mod_aux(x)(q))
    | [] -> [])
  | Zero ->
    (match y with
    | Pos::q -> union([Zero])(sign_mod_aux(x)(q))
    | Neg::q -> union([Zero])(sign_mod_aux(x)(q))
    | Zero::q -> union([Error])(sign_mod_aux(x)(q))
    | Error::q ->  union([Error])(sign_mod_aux(x)(q))
    | [] -> [])
  | Error -> 
    (match y with
    | h::q -> union([Error])(sign_mod_aux(x)(q))
    | [] -> [])
;;

let rec sign_mod(x:sign list)(y:sign list) : sign list = 
  match x with
  | h::q -> union(sign_mod_aux h y)(sign_mod q y)
  | [] -> []
;;

let rec sign_expr(expr:expr)(sign : (string * (sign list))list) : sign list =
  match expr with
  | Num(x) -> sign_of_num(x)
  | Var(x) -> List.assoc x sign 
  | Op(op,x,y) -> 
    (match op with
      | Add -> sign_add(sign_expr x sign)(sign_expr y sign)
      | Sub -> sign_sub(sign_expr x sign)(sign_expr y sign)
      | Mul -> sign_mul(sign_expr x sign)(sign_expr y sign)
      | Div -> sign_mod(sign_expr x sign)(sign_expr y sign)
      | Mod -> sign_mod(sign_expr x sign)(sign_expr y sign)
    )
;;

let is_Eq(expr1:sign list)(expr2:sign list) : bool =
  if((is_present Pos expr1 && is_present Pos expr2) || (is_present Zero expr1 && is_present Zero expr2) || (is_present Neg expr1 && is_present Neg expr2)) then
    true
  else false
;;

let is_Ne(expr1:sign list)(expr2:sign list) : bool =
  is_Eq(expr1)(expr2) = false
;;

let is_Lt(expr1:sign list)(expr2:sign list) : bool =
  if((is_present Pos expr2 && (is_present Pos expr1 || is_present Zero expr1 || is_present Neg expr1)) || (is_present Zero expr2 && is_present Neg expr1) || (is_present Neg expr2 && is_present Neg expr1)) then
    true
  else
    false
;;

let is_Le(expr1:sign list)(expr2:sign list) : bool =
  is_Eq(expr1)(expr2) || is_Lt(expr1)(expr2)
;;

let is_Gt(expr1:sign list)(expr2:sign list) : bool =
  if((is_present Pos expr2 && is_present Pos expr1) || (is_present Zero expr2 && is_present Pos expr1) || (is_present Neg expr2 && (is_present Neg expr1 || is_present Zero expr1 || is_present Pos expr1)))then
    true
  else
    false
;;

let is_Ge(expr1:sign list)(expr2:sign list) : bool =
  is_Eq(expr1)(expr2) || is_Gt(expr1)(expr2)
;; 

let sign_cond(cond:cond)(sign: (string * (sign list))list) : bool = 
  let sign_expr1 = sign_expr cond.expr1 sign in
  let sign_expr2 = sign_expr cond.expr2 sign in
  match cond.comp with
  | Eq -> is_Eq (sign_expr1)(sign_expr2)
  | Ne -> is_Ne(sign_expr1)(sign_expr2)
  | Le -> is_Le(sign_expr1)(sign_expr2)
  | Lt -> is_Lt(sign_expr1)(sign_expr2)
  | Ge -> is_Ge(sign_expr1)(sign_expr2)
  | Gt -> is_Gt(sign_expr1)(sign_expr2)
;;

let rec mise_a_jour_sign (sign:(string * sign list)list)(name:string)(newsign:sign list) : (string * sign list)list=
  match sign with
  |[] -> (name,newsign)::[]
  |(nom,s)::q -> 
      if (nom=name) then 
        (nom,newsign)::q 
      else 
        (nom,s)::mise_a_jour_sign q name newsign 
;;
let rec sign_Lt(y:sign list) : sign list =
  match y with
  | Pos::q -> union([Pos;Neg;Zero])(sign_Lt(q))
  | Neg::q -> union([Neg])(sign_Lt(q))
  | Zero::q -> union([Neg])(sign_Lt(q))
  | Error::q -> union([Error])(sign_Lt(q))
  | [] -> []
;;

let rec sign_Gt(y:sign list) : sign list =
  match y with
  | Pos::q -> union([Pos])(sign_Gt(q))
  | Neg::q -> union([Pos;Neg;Zero])(sign_Gt(q))
  | Zero::q -> union([Pos])(sign_Gt(q))
  | Error::q -> union([Error])(sign_Gt(q))
  | [] -> []
;;

let get_inverse_comp(cond:cond) : cond = 
  match cond.comp with
  | Eq -> { comp = Ne; expr1 = cond.expr1; expr2 = cond.expr2}
  | Ne -> { comp = Eq; expr1 = cond.expr1; expr2 = cond.expr2}
  | Ge -> { comp = Lt; expr1 = cond.expr1; expr2 = cond.expr2}
  | Gt -> { comp = Le; expr1 = cond.expr1; expr2 = cond.expr2}
  | Le -> { comp = Gt; expr1 = cond.expr1; expr2 = cond.expr2}
  | Lt -> { comp = Ge; expr1 = cond.expr1; expr2 = cond.expr2}
;;

let propag_cond(cond:cond)(sign: (string * (sign list))list) : (string * (sign list))list =
  match cond.expr1 with
  | Var(x) -> if(sign_cond cond sign)then
                let sign_expr1 = sign_expr cond.expr1 sign in
                let sign_expr2 = sign_expr cond.expr2 sign in
                match cond.comp with
                  | Eq -> mise_a_jour_sign(sign)(x)(intersect sign_expr1 sign_expr2)
                  | Ne -> mise_a_jour_sign(sign)(x)(contraire sign_expr1 sign_expr2) 
                  | Le -> mise_a_jour_sign(sign)(x)(union(intersect(sign_expr1)(sign_expr2))(intersect(sign_expr1)(sign_Lt sign_expr2)))
                  | Lt -> mise_a_jour_sign(sign)(x)(intersect(sign_expr1)(sign_Lt sign_expr2))
                  | Ge -> mise_a_jour_sign(sign)(x)(union(intersect(sign_expr1)(sign_expr2))(intersect(sign_expr1)(sign_Gt sign_expr2)))
                  | Gt -> mise_a_jour_sign(sign)(x)(intersect(sign_expr1)(sign_Gt sign_expr2))
              else
                sign
  | _ -> sign
;;

let rec union_sign (l1:(string * (sign list)) list) (l2:(string * (sign list)) list) =
  let rec f x l = 
  match l with
    | [] -> true
    | (name,sign)::tl ->
      if x = name then false else f x tl
   in
  match l2 with
  | [] -> l1
  | (name,sign)::tl ->
    if f name l1 then
      union_sign((name,sign)::l1) tl
    else
      union_sign(mise_a_jour_sign(l1)(name)(sign))(tl)
;;

let rec sign_block(block:block)(sign:(string * (sign list)) list) : (string * (sign list))list = 
  match block with
  | [] -> sign
  | (pos,instr)::liste -> let s = sign_instr instr sign in
                          union_sign(s)(sign_block liste s)
and sign_instr(instr:instr)(sign:(string * (sign list)) list) : (string * (sign list))list =
  match instr with
  | Set(name,expr) -> mise_a_jour_sign(sign)(name)(sign_expr expr sign)
  | Read(name) -> mise_a_jour_sign(sign)(name)([Pos;Neg;Zero])
  | Print(expr) -> sign
  | If(cond,blockif,blockelse) -> 
    if(sign_cond cond sign)then
      union_sign(sign_block blockif sign)(sign_block blockelse sign)
    else
      sign_block(blockelse)(propag_cond(get_inverse_comp cond)(sign))
  | While(cond,block) -> 
    if(sign_cond cond sign)then
      let s = sign_block(block)(propag_cond cond sign) in
      let s2 = sign_block(block)(propag_cond cond s) in
      if(s = s2)then
        s
      else
        sign_block(block)(propag_cond cond s2)
    else
      sign
;;

let rec is_error(sign:(string * (sign list)) list) : bool * string =
  match sign with
  | (name,s)::q -> if(is_present Error s) then true,name
                      else is_error q
  | [] -> false,""
;;
