open Def
open String_is_get_read_eval
let rec until_else(l:ligne list)(n:int) : ligne list =(* marche*)
  match l with
  |h::q -> let get = getElement(h.liste) in
      if(get = "ELSE" && h.ind = n ) then h::q
      else until_else q n
  |[] -> []  
;;
let get_block_else (l:ligne list)(n :int) : ligne list = (* marche*)
  let res = until_else(ligne_suive l)(n) in
  get_block res n
;;
let remove_block(l : ligne list)(n : int) : ligne list =
  let res = ligne_suive l in
  let rec remove_block_aux (l : ligne list)(n : int) : ligne list =
    match l with
    | h::q->  if(h.ind > n) then
          remove_block_aux(q)(n)
        else
          h::q
    | [] -> [] 
  in
  remove_block_aux res n
;;

let remove_block_ifelse (l:ligne list)(n: int) : ligne list =
  let l1 = until_else l n in
  remove_block l1 n
;;

let rec read_block (l : ligne list) : block =
  match l with
  | h::v -> let get = getElement(h.liste) in
              if(get = "IF" || get = "WHILE") then
                let cond,liste = read_cond(string_suive h.liste) in
                let block = read_block(get_block(l)(h.ind)) in
                  if(get = "IF") then
                    let blockelse = read_block(get_block_else(l)(h.ind)) in
                    if(get_content blockelse = [])then
                      ({pos = h.ind},If(cond,block,blockelse))::read_block(remove_block(l)(h.ind))
                    else 
                      ({pos = h.ind},If(cond,block,blockelse))::read_block(remove_block_ifelse(l)(h.ind))
                  else
                    ({pos = h.ind}, While(cond,block))::read_block(remove_block(l)(h.ind))
              else
                if(get = "ELSE") then
                  read_block(v)
                else
                  let instr,slist = read_instr(h.liste) in
                  ({pos = h.ind},instr)::read_block v
              
                
  | [] -> []
;;
