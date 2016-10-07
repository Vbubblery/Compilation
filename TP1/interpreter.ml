open Ast

let rec interpret_expr (e : Ast.expr) : int =
match e with
|Eint n-> n
|Ebinop (op, e1,e2)-> 
let r1=interpret_expr(e1) in
let r2=interpret_expr(e2) in

match op with
|Plus ->r1+r2
|Minus ->r1-r2
|Mult ->r1*r2
|Div ->r1/r2
