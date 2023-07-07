(*David Pulido Cornejo 21209533*)

(*NOUVEAUX MOTS EVALUES DANS LE STACK*)

(*
  Sur papier : 
  TUCK (a b c -- a c c b) 
  insere deux fois l'(ancien) sommet de la pile 
  entre les elements 2 et 3 de la pile

  9 12 15 TUCK
  execution :
  STACK       NEXT LINE
  15 12 9     DUP
  15 15 12 9  ROT
  15 12 15 9  SWAP
  12 15 15 9  -           (<-- resultat 12 etant le sommet de la pile)

  ------------
  CUBE (n1 -- n2) tel que n2 = n1 * n1 * n1
  6 CUBE
  execution :
  STACK       NEXT LINE
  6           DUP
  6 6         CARRE
  6 6         DUP
  6 6 6       *
  36 6        *
  216         -          (<-- resultat)
*)

(* *********** Question 1.a *********** *)

exception Empty_stack;;
exception Runtime_error;;

type bin_op = LESS_T | GREATER_T | EQUALS | DIFF
            | PLUS | MINUS | MULT | DIV;; 
type stack_op = DUP | DROP | SWAP | ROT;;

type operation = BINARY of bin_op
               | STACK of stack_op ;;
               
type constant = BOOL of bool | INT of int;;
 
type element = OP of operation | CST of constant | MOT of string ;; 

(* *********** Question 1.b *********** *)

let to_string (x:element) : string = 
  match x with
  | OP(STACK(DUP)) -> "DUP"
  | OP(STACK(DROP)) -> "DROP"
  | OP(STACK(SWAP)) -> "SWAP"
  | OP(STACK(ROT)) -> "ROT"
  | OP(BINARY(PLUS)) -> "+"
  | OP(BINARY(MINUS)) -> "-"
  | OP(BINARY(DIV)) -> "/"
  | OP(BINARY(MULT)) -> "*"
  | OP(BINARY(LESS_T)) -> "<"
  | OP(BINARY(GREATER_T)) -> ">"
  | OP(BINARY(EQUALS)) -> "=" 
  | OP(BINARY(DIFF)) -> "<>" 
  | MOT(str) -> str
  | CST(INT(n)) -> string_of_int n
  | CST(BOOL(true)) -> "TRUE"
  | CST(BOOL(false)) -> "FALSE";; 

let of_string (s:string) : element =
  match s with
  | "DUP" -> OP(STACK(DUP)) 
  | "DROP" -> OP(STACK(DROP)) 
  | "SWAP" -> OP(STACK(SWAP)) 
  | "ROT" -> OP(STACK(ROT)) 
  | "+" -> OP(BINARY(PLUS))
  | "-" -> OP(BINARY(MINUS))
  | "/" -> OP(BINARY(DIV))
  | "*" -> OP(BINARY(MULT))
  | "<" -> OP(BINARY(LESS_T))
  | ">" -> OP(BINARY(GREATER_T))
  | "=" -> OP(BINARY(EQUALS))
  | "<>" -> OP(BINARY(DIFF))
  | "TRUE" -> CST(BOOL(true))
  | "FALSE" -> CST(BOOL(false))
  | _ -> 
      try 
        CST(INT(int_of_string s))
      with 
        _ -> MOT(s);;


(* *********** Question 1.c *********** *)

(** fonction utilitaire : 
    [split s] découpe le texte [s] en une suite de mot. 
*)
let split (s:string) : string list =
  (* traite les tabulations et les sauts de lignes comme des espaces *)
  let normalize_s = String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s in
  let split_s = String.split_on_char ' ' normalize_s in
  (* ignore les espaces successifs *)
  List.filter ((<>) "") split_s ;;

assert (split "  \t  \n " = []) ;;
assert (split " A     \n B\t C  " = ["A";"B";"C"]) ;;

(** transforme un texte (représentant un programme ou une pile)
    en une suite de symboles du langage (e.g., "42" et "DUP") 
*)
let parse (s:string) : element list =
  let lst_mots = split s in
  let rec loop (lst: string list): element list = 
    match lst with 
    | [] -> []
    | mot::r -> (of_string mot)::(loop r)
  in
  loop lst_mots;;

(** transforme un suite de symbole du langage (représentant un programme 
    ou une pile) en un texte équivalent. 
    Par exemple : [text (parse "1 2 +")) = "1 2 +"].
*)
let rec text (p:element list) : string =
  match p with
  | [] -> ""
  | elt::r -> 
      let text_acc = text r in
      if text_acc = "" then to_string elt
      else (to_string elt) ^ " " ^ text_acc;;

(* *********** Question 2 *********** *)

type prog = element list;;
type stack = element list;;

(* FONCTIONS AUXILIERES D'EVALUATION *)

(*Limitations : on ne peut pas comparer deux booleans*)
let eval_binop (stk:stack) (op:bin_op): stack =
  (*Extraction des deux constantes dans le sommet de la pile*)
  let (val1,val2, stk') = match stk with
    | CST(INT(x))::CST(INT(y))::r -> (x,y,r) 
    | _::_::r -> raise(Invalid_argument "eval_binop")
    | _ -> failwith "Error : stack too small"
  in
  (*Evaluation de l'operateur*)
  let res = match op with
    | EQUALS -> (CST(BOOL(val1 = val2)))
    | GREATER_T -> (CST(BOOL(val1 < val2)))
    | LESS_T -> (CST(BOOL(val1 > val2)))
    | DIFF -> (CST(BOOL(val1 <> val2))) 
    | PLUS -> (CST(INT(val1 + val2)))
    | MINUS -> (CST(INT(val2 - val1)))
    | MULT -> (CST(INT(val1 * val2)))
    | DIV -> (CST(INT(val2 / val1))) 
  in res::stk';;

(*Evaluation des operation de stack tq il est precise dans l'enonce*)
let eval_stackop (stk:stack) (op:stack_op) : stack = 
  match op with
  | DUP -> 
      (match stk with
       | elt::r -> elt::stk
       | _ -> raise(Empty_stack))
  | DROP -> 
      (match stk with
       | elt::r -> r
       | _ -> raise(Empty_stack))
  | SWAP -> 
      (match stk with
       | e1::e2::r -> e2::e1::r
       | _ -> raise(Empty_stack))
  | ROT -> 
      (match stk with
       | a::b::c::r -> b::c::a::r
       | _ -> raise(Empty_stack));;

(* [step stk e] exécute l'élément [e] dans la pile [stk] 
   et retourne la pile résultante *)
let step (stk:stack) (e:element) : stack =
  match e with
  | CST(valeur) -> e::stk
  | OP(op_type) -> 
      (match op_type with 
       | STACK(op) -> eval_stackop stk op 
       | BINARY(op) -> eval_binop stk op) 
  | MOT(_) -> raise(Invalid_argument "step");;
    
(* *********** Question 3 *********** *)

let rec calc (stk:stack) (p:prog) : stack =
  match p with
  | [] -> stk
  | elt::r -> calc (step stk elt) r;;

(* *********** Question 4 *********** *)

type name = string;;
(*Representation par ABR*)
type dico = Empty | Dico of ((name * prog) * dico * dico);;

let empty = Empty;;

(*Insertion classique dans un ABR*)
let rec add (nom:name) (def:prog) (dic:dico): dico = 
  match dic with
  | Empty -> Dico((nom,def), Empty, Empty)
  | Dico((nom',def'),fg,fd) ->
    if nom < nom' then 
      Dico((nom',def'), add nom def fg, fd)
    else if nom > nom' then
      Dico((nom',def'), fg, add nom def fd)
    else (*si le nom existe deja on met a jour la definition du mot*)
      Dico((nom,def),fg,fd);;

(*Recherche classique*)
let rec lookup (nom:name) (dic:dico): prog = 
  match dic with 
  | Empty -> raise(Not_found)
  | Dico((nom',def'),fg,fd) -> 
    if nom = nom' then def'
    else if nom < nom' then lookup nom fg 
    else lookup nom fd;;

(* *********** Question 5 *********** *)

let extract_new_definition (nom:name) (prg:prog) (dic:dico): (dico * prog) = 
  let rec loop (prg':prog) (def:prog) (depth:int): (dico*prog) =
    match prg' with
    | [] -> raise(Failure "extract_new_definition")
    | MOT(";")::r -> 
        if depth = 0 then (add nom (List.rev def) dic),r
        else loop r (MOT(";")::def) (depth - 1)
    | MOT(":")::r -> loop r (MOT(":")::def) (depth + 1)
    | expr::r -> loop r (expr::def) depth
  in
  loop(prg) [] 0;;

(*Filtrage du segment TEXT (prog) du programme en fonction du resultat
   de l'evaluation de la condition avant IF (stocke dans b:bool)*)
let filter_out_if (b:bool) (prg:prog) : prog = 
  (*
  toggle indique le mode du parcours : il nous dit si on prend le bloc courant ou non
  depth nous dit le niveau d'imbrication
  *)
  let rec loop (prg':prog) (toggle:bool) (acc:prog) (depth:int) : prog = 
    match prg' with
    | [] -> raise(Failure "filter_out_if")
    | MOT("THEN")::r ->
        (*chaque then et endif decremente le niveau d'imbrication*)
        if depth = 0 then 
          (List.rev acc)@r 
        else if toggle then
          loop r toggle (MOT("THEN")::acc) (depth - 1)
        else 
          loop r toggle acc (depth - 1)
    | MOT("ENDIF")::r ->
        if depth = 0 then 
          (List.rev acc)@r 
        else if toggle then
          loop r toggle (MOT("ENDIF")::acc) (depth - 1)
        else 
          loop r toggle acc (depth - 1)
    | MOT("ELSE")::r -> 
        (*s'il s'agit du niveau actuel alors on change le mode du parcours*)
        if depth = 0 then 
          loop r (not toggle) acc 0
        else if toggle then
          loop r toggle (MOT("ELSE")::acc) depth
        else 
          loop r toggle acc depth
    | MOT("IF")::r -> 
        (*chaque if augmente le niveau d'imbrication*)
        if toggle then
          loop r toggle (MOT("IF")::acc) (depth + 1)
        else 
          loop r toggle acc (depth + 1)
    | expr::r -> 
        if toggle then
          loop r toggle (expr::acc) depth
        else 
          loop r toggle acc depth

  in
  loop prg b [] 0;;

(*
Evaluation recursive du programme p
On evalue recursivement les nouvelles definitions 
(au lieu de les remplacer par leur definition dans le programme)
On mantient aussi une pile de dictionnaires pour limiter la portee de certaines nouvelles mots
*)
let rec eval (dic:dico) (stk:stack) (p:prog) : stack =
  match p with 
  | [] -> stk
  (*Manipulation des nouvelles definitions*)
  | MOT(":")::MOT(";")::r | MOT(":")::MOT(_)::MOT(";")::r -> 
      (*on pourrait faire aussi eval dic stk r pour ignorer les def vides*)
      raise(Failure "eval") 
  | MOT(":")::MOT(nom)::r -> 
      let (new_dico,new_prg) = extract_new_definition nom r dic in
      eval new_dico stk new_prg 
  (*Execution de IF*)
  | MOT("IF")::r -> 
      let (value, new_stack) = match stk with 
        | CST(BOOL(b))::r -> (b,r)
        | _ -> raise(Failure "eval")
      in
      (*filtrage des blocks apres avoir evalue la condition*)
      let prg = filter_out_if value r in 
      (*suite d'execution*)
      eval dic new_stack prg
  (*Execution de nouvelles mots*)
  | MOT(nom)::remainder ->
      (*recherche du mot*)
      let prg = (lookup nom dic) in
      (*On evalue recursivement le mot a partir de la pile actuel et on empile une nouvelle couche dans la pile des dico*)
      let new_stack = eval dic stk prg in 
      (*On continue l'execution en fonction de la nouvelle pile*)
      eval dic new_stack remainder 
  | elt::r -> eval dic (step stk elt) r;;


(* *********** Question 6 *********** *)

let carre n = 
  Printf.sprintf ": carre DUP * ; %d carre" n

(*pour eviter boucles inf. alors fib n = 0 si n < 1*)
let fib n = 
  Printf.sprintf 
    ": FIB DUP 1 < IF DROP 0 ELSE DUP 1 = IF ELSE DUP 1 - FIB SWAP 2 - FIB + THEN THEN ; %d FIB" 
    n;;

(* *********** Question 7 *********** *)

let jeux_de_test = [ 
    (": FACT DUP 1 > IF DUP 1 - FACT * THEN ; 6 FACT", "720");
    (": FIB DUP 1 < IF DROP 0 ELSE DUP 1 = IF ELSE DUP 1 - FIB SWAP 2 - FIB + THEN THEN ; 10 FIB", "55");
    (": SUM DUP 1 > IF DUP 1 - SUM + THEN ; 10 SUM", "55")
  ];;



(* *** EXTRA : Interaction avec l'utilisateur ** *)
(*Lecture ligne par ligne depuis le terminal*)
let lineInterpreter() =
  let _ = print_string "PF23 interpreter - version 0.9" ; print_newline() in
    let rec loop (stk:stack) : int = 
      let _ = print_string ">>> " in 
      let code = read_line() in 
      if code = "quit" || code = "exit" then 
        0
      else 
        let new_stk = eval empty stk (parse code) in 
        let txt = text new_stk in 
        let _ = print_string ("[stk] " ^ txt ^ "\n") in 
        loop new_stk
    in
    try  
      loop []
    with 
      | _ -> let _ = print_string "Syntax error\n" in loop [];;

(* Lecture et execution d'un fichier *)
let fileReader() =
  (*manipulation des fichiers sur stackoverflow.com ...*)    
  let chan = open_in_bin Sys.argv.(1) in
  let contents = really_input_string chan (in_channel_length chan) in
  let _ = close_in chan in 
  let stk = 
    try eval empty [] (parse contents) with | _ -> raise(Runtime_error) (*Peut etre plus tard distinction des cas plus precises*)
  in
  let txt = text stk in
  let _ = print_string ("[stk] " ^ txt ^ "\n") in 
  0 

let main() =
  if (Array.length Sys.argv) = 1 then 
    lineInterpreter()
  else 
    try fileReader() with | _ -> let _ = print_string "Runtime error\n" in -1;;

(*Enlever commentaire pour tester l'interpretation*)
main();;