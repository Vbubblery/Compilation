{
  open Lexing
  open Parser
  open Ast

    (* mis à jour le compteur de la ligne*)
  let maj_pos lexbuf =
    (*position actuelle*)
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      (*incrémenter le compteur*)
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }




  (* Vous pouvez insÃ©rer ici du code Caml pour dÃ©finir des fonctions
     ou des variables qui seront utilisÃ©es dans les actions sÃ©mantiques. *)
  
  (*ajouter*)
  let level = ref 0 ;;
 (* 
    + : nb quelconque au moins 1
    * : nb qq éventuellement 0 
  *)
}
let digit  = ['0'-'9']
let number = digit+



rule token = parse
  | '\n' (* Actions en cas de retour Ã  la ligne :
	    1. enregistrer le changement de ligne dans le tampon [lexbuf]
	    (utile pour localiser les erreurs)
	    2. relancer l'analyse en rappelant la fonction [token] sur [lexbuf]
	 *)
      { maj_pos lexbuf; new_line lexbuf; token lexbuf }

      
  |[' ' '\t']
      { token lexbuf }
  (*eof : la fin du fichier*)
  | eof  (* Fin de fichier : admission du lexeme [EOF] *)
      { EOF }
  (*constante entiers *)
  | number   (*lexeme : chaine de caractère entier corespondant *)
      {
        try
        INT(int_of_string (lexeme (* Lexing.lexbuf -> string*) lexbuf))
      with Failure _ ->
        failwith "integer constante invalide"
      }
  


  (*1.2 commentaires imbriqués*)
  | "(*"  { comment lexbuf; token lexbuf }
      


  (*les opérateurs*)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  
  (*paranthèses*)
  | '(' { LPAR }
  | ')' { RPAR }

  (*comparaison*)
  (*égalité ==, d'inégalité !=, et de comparaison <, <=, > et >=.*)

  | '<' { LT }
  | '>' { GT }
  | "<=" { LE  }
  | ">=" { GE  }
  | "!=" { NEQ }
  | "==" { EQ }


  (*des opérations booléennes &&, || et not*)
  | "&&" { AND }
  | "||" { OR  }
  | "not"  { NOT }
  | "-" { UMINUS }

  (*les constantes booléennes true et false.*)
  |"true" { BOOL(true)  }
  |"false" { BOOL(false) }

  (*des branchements conditionnels if then else.*)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE  }

  | _
      { failwith "Lexical error" }
      
and comment = parse 
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf}
  |  _ { comment lexbuf }
  | eof { failwith "commentaire non terminé" }
      
