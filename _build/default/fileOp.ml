open Def
open String_is_get_read_eval
let rec print_list (liste: string list) : unit= 
  match liste with
  | []-> ()
  | h::q -> print_string(h); print_string(" "); print_list(q);
;;
let print_ligne (ligne: ligne) : unit =
  print_string(string_of_int(ligne.ind)); print_list(ligne.liste)
;;
let rec print_liste_ligne (liste : ligne list) : unit =
  match liste with
  | [] -> ()
  | h::q -> print_ligne(h); print_string("\n"); print_liste_ligne(q)
;;
let create_ligne (s:string) : ligne =
  let liste = String.split_on_char ' ' s in 
  let indentation = get_ind liste in 
  let ligne = { ind = indentation; liste = liste} in
  ligne
;; 
let rec clean (liste: ligne list) : ligne list =
    match liste with
      | [] -> []
      | h::q -> 
      if(getElement(h.liste) = "COMMENT")then
        clean(q)
      else
        h::clean(q)
;;
let rec liste_ligne (liste: string list) : ligne list =
    match liste with
    | h::q -> create_ligne(h)::liste_ligne(q) 
    | [] -> []
;;
let rec clean_string_list (liste: string list) : string list =
  match liste with
  | h::q when h = "" || h = " "-> clean_string_list(q)
  | h::q -> h::clean_string_list(q)
  | [] -> []
;;
let rec clean_ligne_list (liste: ligne list) : ligne list =
  match liste with
  | h::q -> {ind = h.ind; liste = clean_string_list(h.liste)}::clean_ligne_list q
  | [] -> []
;;
let ligne_suive (liste: ligne list) : ligne list =
  match liste with
  | h::q -> q
  | [] -> []
;;
let file_to_ligne (filename:string) : ligne list =
  let f = open_in filename in
  let rec dico_rec () : ligne list =
    try
      let s = input_line f in
      let ligne = create_ligne s in
      if(getElement ligne.liste = "COMMENT")then
        dico_rec()
      else
        ligne::dico_rec()
    with 
      End_of_file -> []
  in dico_rec() ;;
;;

let final_ligne(filename:string) : ligne list =
  let file = file_to_ligne filename in
  clean_ligne_list file
;;
