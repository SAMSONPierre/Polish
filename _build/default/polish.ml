open Def
open String_is_get_read_eval
open Simpl
open Vars
open Sign
open FileOp
open BoucleOp
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
(**********************MAIN****************************)

let read_polish (filename:string) : program = 
  let liste = final_ligne filename in
  read_block liste
  
;;
let print_polish (p:program) : unit =
  print_string(string_of_program p)
;;

let eval_polish (p:program) : unit = 
  eval_instr [] p
;;

let simpl_polish (p:program) : program =
  deadcode(simpl_block p)
;;

let var_polish(p:program) : unit = 
  let all_vars = liste_var_block(p)(Var.empty) in
  let not_init_vars,init = liste_not_init_var_block(p)(Var.empty)(Var.empty) in
  print_string("all var : "); print_list(Var.elements all_vars); print_string("\n");
  print_string("not initialized var : "); print_list(Var.elements not_init_vars); print_string("\n");
;;

let sign_polish(p:program) : unit =
  let s = sign_block p [] in
  print_string(print_sign_var s);
  let b,name = is_error s in
  if(b)then
    print_string("Division par 0 avec var : " ^ name ^ "\n")
  else
    print_string("Pas d'erreur\n")
;;
  
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"
;;

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | [|_;"--simpl";file|] -> print_polish (simpl_polish(read_polish file))
  | [|_;"--vars";file|] -> var_polish(read_polish file)
  | [|_;"--sign";file|] -> sign_polish(read_polish file)
  | _ -> print_polish(read_polish "exemples/fact.p")
;;
(*lancement de ce main *)
let () = main ();;

(***********************************************************************)
