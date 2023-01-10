open Def
let rec string_of_ind (ind: int) : string =
  if(ind <> 0)then
    "  " ^ string_of_ind(ind-1)
  else
   ""
 ;;
let get_ind_of_block (block : block) : string =
  match block with
  | (pos,instr)::q -> string_of_ind (pos.pos-1)
  | _ -> failwith "erreur"
;;
let string_suive (liste:string list) : string list =
  match liste with
  |h::q -> q
  | [] -> []
;;
let string_of_op (op:op) : string = 
  match op with
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " % "
;;


let rec string_of_expr (expr:expr) : string =
  match expr with
  | Num(x) -> string_of_int(x)
  | Var(x) -> x
  | Op(x,y,z) -> "(" ^ string_of_expr y ^ string_of_op x ^ string_of_expr z ^ ")"
;;

let string_of_comp (comp:comp) : string =
  match comp with
  | Eq -> " = "
  | Ne -> " <> "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "
;;

let string_of_cond (cond:cond) : string =
  string_of_expr cond.expr1 ^ string_of_comp cond.comp ^ string_of_expr cond.expr2
;;

let get_content(block:block) : block =
  match block with
  | [] -> []
  | _ -> block
let rec string_of_block (block:block) : string = 
  match block with
  | [] -> ""
  | (pos,instruction)::liste -> string_of_ind pos.pos ^ string_of_instr(instruction)  ^ string_of_block(liste)
and string_of_instr (instr:instr) : string =
  match instr with
  | Set(name,expr) -> name ^ " := " ^ string_of_expr expr ^ "\n"
  | Read(name) -> "READ " ^ name ^ "\n"
  | Print(expr) -> "PRINT " ^ string_of_expr expr ^ "\n"
  | If(cond,ifblock,elseblock) -> 
    if(get_content elseblock = [])then
      "IF: " ^ string_of_cond(cond) ^ "\n" ^ string_of_block(ifblock)
      else
      "IF: " ^ string_of_cond(cond) ^ "\n" ^ string_of_block(ifblock) ^get_ind_of_block(ifblock)^ "Else:\n" ^ string_of_block(elseblock) 
  | While(cond,block) -> "While " ^ string_of_cond(cond) ^ "\n" ^ string_of_block(block)
;;

let string_of_program (program:program) : string = 
  string_of_block(program)
;;











let is_op (operation:string) : bool =
  if(operation = "+" || operation = "-" || operation = "*" || operation = "/" || operation = "%") then
    true
  else
    false
;;
  let is_var (var:string) : bool =
    if(var = "PRINT" || var = "READ" || var = "IF" || var = "WHILE" || var = "ELSE") then
      false
    else 
    true
;;
  let is_int (i:string):bool= 
  try
    let rec verif j=
      match i.[j] with 
      |'0' ->true && verif(j+1)
      |'1' ->true && verif(j+1)
      |'2' ->true && verif(j+1)
      |'3' ->true && verif(j+1)
      |'4' ->true && verif(j+1)
      |'5' ->true && verif(j+1)
      |'6' ->true && verif(j+1)
      |'7' ->true && verif(j+1)
      |'8' ->true && verif(j+1)
      |'9' ->true && verif(j+1)
      |_->false
    in
    verif 0
  with
    _->true
;;
let is_comp (comp:string) : bool =
    if(comp = "=" || comp = "<>" || comp = ">" || comp = "<" || comp = ">=" || comp = "<=") then
      true
    else
      false
;;
let rec is_present(elem:sign)(liste:sign list) : bool = 
  match liste with
  | h::q -> if(elem = h)then true
            else is_present(elem)(q)
  | [] -> false 
;;











let ligne_suive (liste: ligne list) : ligne list =
  match liste with
  | h::q -> q
  | [] -> []
;;
let getOP (operation:string) : op =
  match operation with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Erreur"
;;

let getComp (comp:string) : comp =
  match comp with
  | "=" -> Eq
  | "<>" -> Ne
  | ">" -> Gt
  | "<" -> Lt
  | ">=" -> Ge
  | "<=" -> Le
  | _ -> failwith "Erreur"
;;

let getName (liste:string list) : string =
  match liste with
  | n::q -> n
  | _ -> failwith "erreur"
;;

let get_ind (liste:string list) : int =
  let rec get_ind_aux (liste: string list) : int =
    match liste with
    | h::q -> if h = "" then
          1 + get_ind_aux q
        else
          0
    | _ -> 0
  in 
  let res = get_ind_aux liste in 
  res/2
;;
let getElement (liste: string list) : string =
  match liste with
  | h::q -> h
  | [] -> ""

;;

let get_block(l :ligne list)(n : int) : ligne list =(* marche*)
  let l1 = ligne_suive l in
  let rec get_block_aux(l :ligne list)(n : int) : ligne list = 
    match l with
    | h::q -> if h.ind > n then
          h::get_block_aux(q)(n)
        else []
    | [] -> []
  in
  get_block_aux l1 n
;;









let read_comp (comp:string list):comp*(string list) =
  match comp with
  |"="::l->(Eq,l)
  |"<>"::l->(Ne,l) 
  |"<"::l->(Lt,l)
  |"<="::l->(Le,l)
  |">"::l->(Gt,l)
  |">="::l->(Ge,l)
  |_->failwith "ERREUR READ_COMP"
let rec read_expr (l : string list) : expr * (string list) = 
 match l with
  | op::q when is_op op-> let (e1,q') = read_expr q in
             let(e2,q'') = read_expr q' in
             (Op(getOP(op),e1,e2),q'')
  | n::q when is_int n -> (Num(int_of_string(n)),q)
  | x::q when is_var x -> (Var(x),q)
  | _ -> failwith "ERREUR READ_EXPR"
;;

let read_cond (cond:string list): cond * (string list)=
  try
    let (exp1,l)=read_expr cond in
    let (comp,l2)=read_comp l in
    let (exp2,l3)=read_expr l2 in
    ({expr1 = exp1;comp = comp; expr2 = exp2},l3)
  with
    _->failwith "ERREUR READ_COND"
;;
let read_instr (l : string list) : instr * (string list) =
  match l with
  | "PRINT"::q -> let (e,q') = read_expr(q) in (Print(e),q)
  | "READ"::q -> Read(getName(q)),q
  | n::v::q when v = ":=" -> let (e,q') = read_expr(q) in Set(n,e),q
  | _ -> failwith "ERREUR READ_INSTR"
;;






let eval_op op =
  match op with
  |Add->fun x y -> x+y
  |Sub->fun x y -> x-y 
  |Mul->fun x y -> x*y
  |Div->fun x y -> x/y
  |Mod->fun x y -> x mod y
;;
let rec eval_expr(expr:expr)(env:(string * int) list) : int=
  match expr with
  |Num(x)->x
  |Var(x)->List.assoc x env
  |Op(op,expr1,expr2)->eval_op op (eval_expr expr1 env)(eval_expr expr2 env)
;;
let eval_cond (cond:cond)(env:(string*int)list):bool =
  match cond.comp with
  |Eq->eval_expr(cond.expr1)(env) = eval_expr(cond.expr2)(env)
  |Ne->eval_expr(cond.expr1)(env) <> eval_expr(cond.expr2)(env)
  |Lt->eval_expr(cond.expr1)(env) < eval_expr(cond.expr2)(env)
  |Le->eval_expr(cond.expr1)(env) <= eval_expr(cond.expr2)(env)
  |Gt->eval_expr(cond.expr1)(env) > eval_expr(cond.expr2)(env)
  |Ge->eval_expr(cond.expr1)(env) >= eval_expr(cond.expr2)(env)
;;
let rec mise_a_jour_env (env:(string * int)list)(name:string)(v:int) : (string * int)list=
  match env with
  |[]->(name,v)::[]
  |(nom,value)::b-> 
      if (nom=name) then 
        (nom,v)::b
      else 
        (nom,value)::mise_a_jour_env b name v 
;;

let rec concatene_block(block1:block)(block2:block) : block =
  match block1 with
  | [] -> block2
  | x::q -> x::concatene_block q block2
;;


let rec eval_instr(env:(string * int)list)(block:block) : unit =
  match block with
  |[]->()
  |(pos,instr)::q->
    match instr with 
    |Set(name,expr)->
      let environnement = mise_a_jour_env(env)(name)(eval_expr(expr)(env)) in
      eval_instr environnement q
    |Read(name)-> print_string(name ^ " ?\n ");
      let n=read_int() in 
      let environnement= mise_a_jour_env env name n in
      eval_instr environnement q
    |Print(expr)-> print_string(string_of_expr(expr) ^ " : " ^ string_of_int(eval_expr(expr)(env)) ^ "\n");
                    eval_instr env q
    |If(cond,block1,block2)-> if(eval_cond(cond)(env))then
                                let suite = concatene_block block1 q in 
                                eval_instr(env)(suite)
                              else if(get_content block2 = [])then
                                eval_instr env q
                              else
                                let suite = concatene_block block2 q in 
                                eval_instr(env)(suite)
    |While(cond,block)-> if(eval_cond(cond)(env))then
                            let suite = concatene_block(block)((pos,instr)::q)  in 
                              eval_instr(env)(suite)
                            else
                              eval_instr(env)(q)
;;
