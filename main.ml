(*David Pulido Cornejo 21209533*)

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

type operation = BINARY of string
               | STACK of string ;;
               
type constant = BOOL of bool | INT of int | STR of string;;
 
type element = OP of operation | CST of constant | MOT of string ;; 

(* *********** Question 1.b *********** *)

let to_string (x:element) : string = 
  match x with
  | OP(STACK(str)) -> str
  | OP(BINARY(str)) -> str
  | MOT(str) -> str
  | CST(INT(n)) -> string_of_int n
  | CST(BOOL(true)) -> "TRUE"
  | CST(BOOL(false)) -> "FALSE"
  | CST(STR(str)) -> str;;

let of_string (s:string) : element =
  match s with
  | "DUP" | "DROP" | "SWAP" | "ROT" 
  | "dup" | "drop" | "swap" | "rot" 
  | "empty?" | "EMPTY?" -> OP(STACK(s))  
  | "+" | "-" | "/" | "*"  
  | "<" | ">" | "=" | "<>"-> OP(BINARY(s))
  | "TRUE"  | "true"-> CST(BOOL(true))
  | "FALSE" | "false" -> CST(BOOL(false))
  | _ -> 
      if String.get s 0 = '"' 
        && String.get s (String.length s - 1) = '"' 
        && String.length s <> 1 then 
        CST(STR(s))
      else 
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

let eval_binop (stk:stack) (op:string): stack =
  (*Extraction des deux constantes dans le sommet de la pile*)
  match stk with
    (*CAS operation entre 2 entiers*)
    | CST(INT(val1))::CST(INT(val2))::stk' ->  
      (*Evaluation de l'operateur*)
      let res = match op with
        | "=" -> (CST(BOOL(val1 = val2)))
        | ">" -> (CST(BOOL(val1 < val2)))
        | "<" -> (CST(BOOL(val1 > val2)))
        | "<>" -> (CST(BOOL(val1 <> val2))) 
        | "+" -> (CST(INT(val1 + val2)))
        | "-" -> (CST(INT(val2 - val1)))
        | "*" -> (CST(INT(val1 * val2)))
        | "/" -> 
          if val1 = 0 then raise(Invalid_argument "eval_binop")
          else (CST(INT(val2 / val1))) 
        | _ -> raise(Invalid_argument "eval_binop")
      in 
      res::stk'
    (*CAS comparaison 2 booleens*)
    | CST(BOOL(val1))::CST(BOOL(val2))::stk' ->  
      (*Evaluation de l'operateur*)
      let res = match op with
        | "=" -> (CST(BOOL(val1 = val2)))
        | "<>" -> (CST(BOOL(val1 <> val2))) 
        | _ -> raise(Invalid_argument "eval_binop")
      in 
      res::stk'
    | _::_::r -> raise(Invalid_argument "eval_binop")
    | _ -> failwith "Error : stack too small";;

(*Evaluation des operation de stack tq il est precise dans l'enonce*)
let eval_stackop (stk:stack) (op:string) : stack = 
  match op with
  | "DUP" | "dup" -> 
      (match stk with
       | elt::r -> elt::stk
       | _ -> raise(Empty_stack))
  | "DROP" | "drop" -> 
      (match stk with
       | elt::r -> r
       | _ -> raise(Empty_stack))
  | "SWAP" | "swap" -> 
      (match stk with
       | e1::e2::r -> e2::e1::r
       | _ -> raise(Empty_stack))
  | "ROT" | "rot" -> 
      (match stk with
       | a::b::c::r -> b::c::a::r
       | _ -> raise(Empty_stack))
  (*test si stk est vide*)
  | "EMPTY?" | "empty?" ->
      (match stk with
         | [] -> CST(BOOL(true))::[]
         | _ -> CST(BOOL(false))::stk)
  | _ -> raise(Invalid_argument "eval_stackop");;

(*Evaluation de print et scan*)
let eval_io (stk:stack) (op:string) :stack = 
  (*cas ecriture...*)
  match op with 
  | "PRINT" | "print" ->
    (match stk with 
        (*extraction du message*)
        | elt::r -> 
          let msg = 
          (match elt with
            | CST(BOOL(true)) -> 
              "true"
            | CST(BOOL(false)) -> 
              "false"
            | CST(INT(x)) -> 
              string_of_int x
            | CST(STR(s)) -> 
              s
            | _ -> 
              "(undefined)")
          in
          (*affichage*)
          let _ = print_string ("Output: "^msg^" \n") in r
        | _ -> raise(Empty_stack) )
  (*Cas lecture...*)
  | "SCAN" | "scan" ->
    let _ = print_string "Input: " in 
    let str = read_line() in
    (of_string str)::stk
  | _ -> raise(Invalid_argument "eval_io");;

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
    | MOT("THEN")::r | MOT("then")::r ->
        (*chaque then et endif decremente le niveau d'imbrication*)
        if depth = 0 then 
          (List.rev acc)@r 
        else if toggle then
          loop r toggle (MOT("THEN")::acc) (depth - 1)
        else 
          loop r toggle acc (depth - 1)
    | MOT("ENDIF")::r | MOT("endif")::r ->
        if depth = 0 then 
          (List.rev acc)@r 
        else if toggle then
          loop r toggle (MOT("ENDIF")::acc) (depth - 1)
        else 
          loop r toggle acc (depth - 1)
    | MOT("ELSE")::r | MOT("else")::r -> 
        (*s'il s'agit du niveau actuel alors on change le mode de la boucle*)
        if depth = 0 then 
          loop r (not toggle) acc 0
        else if toggle then
          loop r toggle (MOT("ELSE")::acc) depth
        else 
          loop r toggle acc depth
    | MOT("IF")::r | MOT("if")::r -> 
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
let rec eval (dic_stk:dico list) (stk:stack) (p:prog) : (stack*dico) =
  (*tete et le reste de la pile de dictionnaires*)
  let (head,body) = match dic_stk with 
    | current::r -> (current,r) 
    | _ -> raise(Invalid_argument "eval")
  in
  match p with 
  | [] -> stk,head
  (*Manipulation des nouvelles definitions*)
  | MOT(":")::MOT(";")::r | MOT(":")::MOT(_)::MOT(";")::r -> 
      (*on pourrait faire aussi eval dic stk r pour ignorer les def vides*)
      raise(Failure "eval") 
  | MOT(":")::MOT(nom)::r -> 
      let (new_dico,new_prg) = extract_new_definition nom r head in
      eval (new_dico::body) stk new_prg 
  (*Execution de IF*)
  | MOT("IF")::r | MOT("if")::r  -> 
      let (value, new_stack) = match stk with 
        | CST(BOOL(b))::r -> (b,r)
        | _ -> raise(Failure "eval")
      in
      (*filtrage des blocks apres avoir evalue la condition*)
      let prg = filter_out_if value r in 
      (*suite d'execution*)
      eval dic_stk new_stack prg
  | MOT("PRINT")::r |  MOT("print")::r ->
      eval dic_stk (eval_io stk "PRINT") r
  | MOT("SCAN")::r |  MOT("scan")::r ->
    eval dic_stk (eval_io stk "SCAN") r     
  (*Execution de nouvelles mots*)
  | MOT(nom)::remainder ->
      (*recherche du mot*)
      let prg = (lookup nom head) in
      (*On evalue recursivement le mot a partir de la pile actuel et on empile une nouvelle couche dans la pile des dico*)
      let new_stack,_ = eval (head::dic_stk) stk prg in 
      (*On continue l'execution en fonction de la nouvelle pile*)
      eval dic_stk new_stack remainder 
  | elt::r -> eval dic_stk (step stk elt) r;;


(* *********** Question 6 *********** *)

let carre n = 
  Printf.sprintf ": carre dup * ; %d carre" n

(*pour eviter boucles inf. alors fib n = 0 si n < 1*)
let fib n = 
  Printf.sprintf 
    ": FIB DUP 1 < IF DROP 0 ELSE DUP 1 = IF ELSE DUP 1 - FIB SWAP 2 - FIB + THEN THEN ; %d FIB" 
    n;;

(* *********** Question 7 *********** *)

let jeux_de_test = [ 
    (*operateurs booleens*)
    (": NOT if FALSE else TRUE endif ; true NOT false NOT", "TRUE FALSE");
    (": AND IF 1 ELSE 0 ENDIF SWAP IF 1 ELSE 0 ENDIF * 1 = ; false false and false true and true false and true true and", 
      "TRUE FALSE FALSE FALSE");
    (": OR if drop true else if true else false endif endif ; false false OR true false OR false true OR true true OR", 
      "TRUE TRUE TRUE FALSE");
    (*comparaison*)
    (": geq dup rot swap dup rot < rot = IF drop true ELSE IF true ELSE false ENDIF ENDIF ; 10 7 geq 10 10 geq 7 10 geq",
      "FALSE TRUE TRUE");
    (*recursivite*)
    (": fact dup 1 > if dup 1 - fact * then ; 6 fact", "720");
    (": SUM DUP 1 > IF DUP 1 - SUM + THEN ; 11 SUM", "66");
    (": MCARTHY  DUP 100 > IF 10 - ELSE 11 + MCARTHY MCARTHY ENDIF ; 87 mcarthy 99 mcarthy", "91 91");
    (*double recursion*)
    (": FIB DUP 1 < IF DROP 0 ELSE DUP 1 = IF ELSE DUP 1 - FIB SWAP 2 - FIB + THEN THEN ; 10 FIB", "55");
    (": ACKER swap dup 0 = IF drop 1 + ELSE swap dup 0 = IF drop 1 swap 1 - swap ACKER ELSE swap dup 1 - rot swap 1 - ACKER ACKER ENDIF ENDIF 
        ; 3 2 acker 2 4 acker", "11 29"); (*Ackerman(m,n) <-> m n ACKER*)
    (*extra*)
    (": FLUSH empty? IF ELSE drop flush ENDIF ; 10 10 20 \"hello\" 1 2 FLUSH", "")
  ];;


(* Ajouter un commentaire (de la ligne 346 a 389) 
  chacher l'interpretation *)

(* *** EXTRA : Interaction avec l'utilisateur ** *)
(*Lecture ligne par ligne depuis le terminal*)
let lineInterpreter() =
  let _ = print_string "PF23 interpreter - version 0.9" ; print_newline() in
    let rec loop (stk:stack) (dic:dico) : int = 
      let _ = print_string ">>> " in 
      let code = String.uppercase_ascii (read_line()) in 
      if code = "QUIT" || code = "EXIT" then 
        0
      else 
        (* on enchaine le dernier etat de la pile
           et le dernier dictionnaire de mots*)
        let new_stk, new_dico = 
          try eval [dic] stk (parse (code)) 
          with 
            | Empty_stack -> let _ = print_string "Error: empty stack\n" in stk,dic
            | _ -> let _ = print_string "Syntax error\n" in stk,dic
        in 
        let txt = text new_stk in 
        let _ = print_string ("[stk] " ^ txt ^ "\n") in 
        loop new_stk new_dico
    in
    loop [] empty;;

(* Lecture et execution d'un fichier *)
let fileReader() =
  (*manipulation des fichiers sur stackoverflow.com ...*)    
  let chan = open_in_bin Sys.argv.(1) in
  let contents = really_input_string chan (in_channel_length chan) in
  let _ = close_in chan in 
  let stk,_ = 
    try eval [empty] [] (parse (String.uppercase_ascii contents)) 
    with | _ -> raise(Runtime_error) 
    (*Peut etre plus tard on pourrait implementer une distinction de cas d'erreurs d'exec plus precise*)
  in
  let txt = text stk in
  let _ = print_string ("[stk] " ^ txt ^ "\n") in 
  0;;

(*Testeur du jeu de tests*)
let rec tester (test_list:(string*string) list): bool =
  match test_list with 
  | [] -> true 
  | (code,res)::body ->
    let stk,_ = 
      try 
        eval [empty] [] (parse (String.uppercase_ascii code)) 
      with | _ -> raise(Runtime_error) 
    in 
    let return = text stk
    in
    (*on compare la pile finale avec le resultat attendu*)
    if return = res then 
      tester(body)
    else
      let 
        _ = print_string ("'"^res^"' expected but '"^return^"' found\n")
      in
      false;; 

let main(): int =
  if (Array.length Sys.argv) = 1 then 
    lineInterpreter()
  else if Sys.argv.(1) <> "test" then 
    try fileReader() with | _ -> let _ = print_string "Runtime error\n" in -1
  else if tester jeux_de_test then 
    let _ = print_string "All tests passed\n" in 0
  else
    let _ = print_string "Aborted\n" in -1;;

main();;
