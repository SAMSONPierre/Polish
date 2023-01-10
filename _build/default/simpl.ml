open Def
let rec inverse_signe (expr:expr) : expr =
  match expr with
  | Num(x) -> Num(-x)
  | Var(x) -> Var("-"^x)
  | Op(op,x,y) -> Op(op,inverse_signe x,inverse_signe y)
let rec simpl_expr (expr:expr) : expr = 
  match expr with
  | Num(x) -> Num(x)
  | Var(x) -> Var(x)
  | Op(op,x,y) -> 
    match op with
    | Add -> if(x = Num(0))then
              simpl_expr(y)
            else if(y = Num(0)) then 
              simpl_expr(x)
            else
              expr
    | Sub -> if(y = Num(0)) then 
              simpl_expr(x)
             else if(x = Num(0) && y != Num(0))then
              simpl_expr(inverse_signe y)
            else
              expr

    | Mul ->if(x = Num(0) || y = Num(0))then
              Num(0)
            else if(x = Num(1)) then 
              simpl_expr(y)
            else if(y = Num(1)) then
              simpl_expr(x)
            else
              expr
    | Div ->if(x = Num(0))then
              Num(0)
            else if(y = Num(0)) then
              simpl_expr(x)
            else 
              expr
    | Mod -> if(y=Num(0))then
              simpl_expr(x)
              else
                expr
;;

let simpl_cond(cond:cond) : cond =
  let res = {
    expr1 = simpl_expr cond.expr1;
    comp = cond.comp;
    expr2 = simpl_expr cond.expr2
  } in 
  res
;;

let rec simpl_block(block:block) : block = 
  match block with
  | [] -> []
  | (pos,instr)::liste -> (pos,simpl_instr instr)::(simpl_block liste)
and simpl_instr(instr:instr) : instr =
  match instr with
  | Set(name,expr) -> Set(name,simpl_expr expr)
  | Read(name) -> Read(name)
  | Print(expr) -> Print(simpl_expr expr)
  | If(cond,blockif,blockelse) ->   If(simpl_cond cond,simpl_block blockif,simpl_block blockelse)
  | While(cond,block) ->  While(simpl_cond cond,simpl_block block)
;;

let rec reduce_ind(block:block) : block =
  match block with
  | [] -> []
  | (pos,instr)::liste -> ({pos = pos.pos-1},instr)::reduce_ind(liste)
;;
let is_Num(expr:expr) : bool =
  match expr with
  | Num(x) -> true
  | _ -> false
;;
let cond_constant(cond:cond) : bool =
  if(is_Num(cond.expr1) && is_Num(cond.expr2))then
    true
  else
    false
;;

let eval_cond_constant(cond:cond) : bool =
  match cond.comp with
  | Eq -> cond.expr1 = cond.expr2
  | Ne -> cond.expr1 <> cond.expr2
  | Lt -> cond.expr1 < cond.expr2
  | Le -> cond.expr1 <= cond.expr2
  | Gt -> cond.expr1 > cond.expr2
  | Ge -> cond.expr1 >= cond.expr2
;;

let rec deadcode(block:block) : block =
  match block with
  | [] -> []
  | (pos,instr)::liste -> 
    match instr with
    | If(cond,blockif,blockelse) -> if(cond_constant cond)then
                                      if(eval_cond_constant cond)then
                                        deadcode(reduce_ind(blockif))@deadcode(liste)
                                      else
                                        deadcode(reduce_ind(blockelse))@deadcode(liste)
                                    else
                                      (pos,If(cond,deadcode blockif,deadcode blockelse))::deadcode(liste)
    | While(cond,block) -> if(cond_constant cond)then
                              if(eval_cond_constant cond)then
                                (pos,While(cond,deadcode block))::deadcode(liste)
                              else
                                deadcode(liste)
                            else
                              (pos,While(cond,deadcode block))::deadcode(liste)                      
    | _ -> (pos,instr)::deadcode(liste) 
;;
