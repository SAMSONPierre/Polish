open Def

let rec liste_var_expr(expr:expr)(set:Var.t) : Var.t = 
  match expr with
  | Num(x) -> set
  | Var(x) -> Var.add x set
  | Op(op,x,y) -> let s = Var.union(set)(liste_var_expr(x)(set)) in
                  let ss = Var.union(s)(liste_var_expr(y)(set)) in
                  ss
;;

let liste_var_cond(cond:cond)(set:Var.t) : Var.t =
  let s = Var.union(set)(liste_var_expr(cond.expr1)(set)) in
  let ss = Var.union(s)(liste_var_expr(cond.expr2)(s)) in
  ss
;;

let rec liste_var_block(block:block)(set:Var.t) : Var.t = 
  match block with
  | [] -> set
  | (pos,instr)::liste -> let s = Var.union(set)(liste_var_instr(instr)(set)) in
                          Var.union(s)(liste_var_block(liste)(set))
and liste_var_instr(instr:instr)(set:Var.t): Var.t =
  match instr with
  | Set(name,expr) -> Var.union(set)(liste_var_expr(expr)(set))
  | Read(name) -> set
  | Print(expr) -> liste_var_expr(expr)(set)
  | If(cond,blockif,blockelse) -> 
      let s = Var.union(set)(liste_var_cond(cond)(set)) in
      let ss = Var.union(s)(liste_var_block(blockif)(s)) in
      let sss = Var.union(ss)(liste_var_block(blockelse)(ss)) in
        sss
  | While(cond,block) -> 
    let s = Var.union(set)(liste_var_cond(cond)(set)) in
    let ss = Var.union(s)(liste_var_block(block)(s)) in
    ss  
;;

let rec is_initialized(var:string)(liste:(string * bool) list) : bool =
  match liste with
  | (elem,bool)::q -> 
    if(var = elem)then
      if(bool)then
        true
      else
        false
    else
      is_initialized(var)(q)
  | [] -> false
;;

let rec liste_not_init_var_expr(expr:expr)(set:Var.t)(init:Var.t) : Var.t =
  match expr with
  | Num(x) -> set
  | Op(op,x,y) -> let s = Var.union(set)(liste_not_init_var_expr(x)(set)(init)) in
                  let ss = Var.union(s)(liste_not_init_var_expr(y)(set)(init)) in
                  ss
  | Var(x) -> 
    try 
      if(Var.find(x)(init) = x) then
        set
      else
        failwith "erreur"
    with 
      _ -> Var.add x set
;;

let liste_not_init_var_cond(cond:cond)(set:Var.t)(init:Var.t) : Var.t =
  let s = Var.union(set)(liste_not_init_var_expr(cond.expr1)(set)(init)) in
  let ss = Var.union(s)(liste_not_init_var_expr(cond.expr2)(s)(init)) in
  ss
;;

let rec liste_not_init_var_block(block:block)(set:Var.t)(init:Var.t) : Var.t * Var.t= 
  match block with
  | [] -> set,init
  | (pos,instr)::liste -> let liste_instr,q = liste_not_init_var_instr(instr)(set)(init) in
                          let liste_block,q' = liste_not_init_var_block(liste)(set)(q) in
                          let s = Var.union(set)(liste_instr) in
                          Var.union(s)(liste_block),q'
and liste_not_init_var_instr(instr:instr)(set:Var.t)(init:Var.t): Var.t * Var.t =
  match instr with
  | Set(name,expr) -> set,Var.add name init
  | Read(name) -> set,Var.add name init
  | Print(expr) -> liste_not_init_var_expr(expr)(set)(init),init
  | If(cond,blockif,blockelse) -> 
      let s = Var.union(set)(liste_not_init_var_cond(cond)(set)(init)) in
      let liste_block_if,q = liste_not_init_var_block(blockif)(s)(init) in 
      let ss = Var.union(s)(liste_block_if) in
      let liste_block_else,q' = liste_not_init_var_block(blockelse)(ss)(init) in
      let sss = Var.union(ss)(liste_block_else) in
        sss,q
  | While(cond,block) -> 
    let s = Var.union(set)(liste_not_init_var_cond(cond)(set)(init)) in
    let liste_block,q = liste_not_init_var_block(block)(s)(init) in
    let ss = Var.union(s)(liste_block) in
      ss,init
;;
