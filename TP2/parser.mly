%{
  
  open Ast
  
(* Vous pouvez insérer ici du code Caml pour dÃ©finir des fonctions
   ou des variables qui seront utilisées dans les actions sémantiques. *)
  
%}
  
(* Definition des lexemes. *)
%token EOF
%token<int> INT
%token PLUS MINUS MULT DIV (*operateurs*)
%token LPAR RPAR (*paranthèse*)


(*comparaison*)
%token EQ NEQ
%token LE LT
%token GE GT 

(*des opérations booléennes &&, || et not*)
%token AND OR
%token NOT
%token UMINUS (*signe < 0*)

(*les constantes booléennes true et false.*)
%token<bool> BOOL

(*des branchements conditionnels if then else.*)
%token IF THEN ELSE 


(* Règles d'associativité et de priorité. *)
%left PLUS MINUS
%left MULT DIV
%left AND OR
%left NOT 
%left UMINUS
%left EQ NEQ LE LT GE GT

 %nonassoc ELSE

(* Symbole de départ et type de la valeur produite par l'action sémantique. *)
%start toplevel_expr
%type <Ast.expr> toplevel_expr

%%

(* Règles. *)
(*<toplevel_expr> ::= <expr> eof*)
toplevel_expr:
(* Expression à  la racine : une expression [expr] dont on notera [e] la valeur
   sémantique, suivie du symbole [EOF].
   La valeur sémantique associée est la valeur [e] de l'expression.
*)
| e=expr; EOF { e };
  
(*<expr>  ::= <int>
  |  ( <expr> )
  |  <expr> <binop> <expr> 
*)
expr:
| i = INT { Econst (Cint i)}
| e1 = expr; op=binop; e2 = expr {Ebinop(op,e1,e2)}
| e1 = expr; op=unop; {Eunop(op,e1)}
| b = BOOL {Econst(Cbool b)}
| IF; e1 = expr; THEN; e2 = expr ELSE e3 = expr {Eif(e1,e2,e3)}
| LPAR; e = expr; RPAR {e}
| EOF { failwith "Unlikely" }
;

%inline binop:
|PLUS {Plus}
|MINUS {Minus}
|MULT {Mult}
|DIV {Div}
|AND {And}
|OR {Or}
|LT {Lt}
|GT {Gt}
|LE {Le}
|GE {Ge}
|NEQ {Neq}
|EQ {Eq}
%inline unop:
|NOT {Not}



nontrivial_condition:
| NOT c = condition
    { Not c }
| c1 = condition AND c2 = condition
    { And (c1, c2) }
| c1 = condition OR c2 = condition
    { OR (c1, c2) }
| LPAR c = nontrivial_condition RPAR
    { c }

condition:
| e = expr
    { Eif (c1,c2,c3) }
| c = nontrivial_condition
    { c }
;;
