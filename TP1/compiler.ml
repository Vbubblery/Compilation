open Ast
(*Delete the ".3.3" of the name and back up another compiler to test the program*)
(*define an argument that can generate the number of $a(num) automaticly *)

(*"declare a function to judge the expr is int or isnt the int"*)

let findEInt e =
  match e with
    |Eint n -> true
    |_-> false
;;
let getTheInt e = 
  match e with
    |Eint n -> n
;;

let numberOfRegistre = ref 0 ;;
let rec compile_expr (e : Ast.expr) : unit = 
  match e with
    |Eint n -> 
      Printf.printf "li $a%i %d\n" !numberOfRegistre n;
      numberOfRegistre := !numberOfRegistre +1;
      (* limit the quantity of the registre which we can
      use. *)
      if !numberOfRegistre > 3 then
        failwith "ops! Something goes wrong, plz try again!"
    |Ebinop (op, e1, e2) ->
      match op with
        |Plus ->
        (*if e1 && e2 ==true, we need only 1 registre to save the data *)
          if (findEInt e1) = true then(
            if(findEInt e2) = true then(
              (*
                Because we used 1 registre to calcule, so we
                dont need to sub the number of registre to the last one
              *)
              compile_expr e1;
              Printf.printf "add $a%i,$a%i,%i\n" (!numberOfRegistre-1) (!numberOfRegistre-1) (getTheInt e2);
            (*if e1 == true but e2 != true, we changed the order of numeration and then use 1 registre to save the result*)
            )else(
              compile_expr e2;
              Printf.printf "add $a%i,$a%i,%i\n" (!numberOfRegistre-1) (!numberOfRegistre-1) (getTheInt e1);
            )
            (*if e1!=true and e2!=true, we dont care the order*)
          )else(
            if(findEInt e2) = false then(
              compile_expr e1;
              compile_expr e2;
              Printf.printf "add $a%i,$a%i,$a%i\n" (!numberOfRegistre-2) (!numberOfRegistre-2) (!numberOfRegistre -1);
              (*Sub the number of the Registre
                because the 2 registres aready add to 
                1 registre, so one registre is free at the moment*)
              numberOfRegistre := !numberOfRegistre - 1;
              (*if e1!=true and e2!=false, we can loop the e1 and save the result to 1 registre and then calculted with e2 directly*)
            )else(
              compile_expr e1;
              Printf.printf "add $a%i,$a%i,%i\n" (!numberOfRegistre-1) (!numberOfRegistre-1) (getTheInt e2);
            )
          );
          (*The same with above*)
        |Minus ->
          if (findEInt e1) = true then(
            if(findEInt e2) = true then(
              compile_expr e1;
              Printf.printf "sub $a%i,$a%i,%i\n" (!numberOfRegistre-1) (!numberOfRegistre-1) (getTheInt e2);
            )else(
              compile_expr e2;
              Printf.printf "sub $a%i,%i,$a%i\n" (!numberOfRegistre-1) (getTheInt e1) (!numberOfRegistre-1);
            )
          )else(
            if(findEInt e2) = true then(
              compile_expr e1;
              Printf.printf "sub $a%i,$a%i,%i\n" (!numberOfRegistre-1) (!numberOfRegistre-1) (getTheInt e2);
            )else(
              compile_expr e1;
              compile_expr e2;
              Printf.printf "sub $a%i,$a%i,$a%i\n" (!numberOfRegistre-2)(!numberOfRegistre-2) (!numberOfRegistre-1);
              numberOfRegistre := !numberOfRegistre - 1;  
            )
          );
        |Mult ->
          if (findEInt e1) = true then(
            if(findEInt e2) = true then(
              compile_expr e1;
              Printf.printf "mul $a%i,$a%i,%i\n" (!numberOfRegistre-1)(!numberOfRegistre-1) (getTheInt e2);
            )else(
              compile_expr e2;
              Printf.printf "mul $a%i,%i,$a%i\n" (!numberOfRegistre-1)(getTheInt e1) (!numberOfRegistre-1);
            )
          )else(
            if(findEInt e2) = true then(
              compile_expr e1;
              Printf.printf "mul $a%i,$a%i,%i\n" (!numberOfRegistre-1)(!numberOfRegistre-1) (getTheInt e2);              
            )else(
              compile_expr e1;
              compile_expr e2;
              Printf.printf "mul $a%i,$a%i,$a%i\n" (!numberOfRegistre-2)(!numberOfRegistre-2) (!numberOfRegistre-1);
              numberOfRegistre := !numberOfRegistre - 1;
            )
          );
        |Div ->
          if (findEInt e1) = true then(
            if(findEInt e2) = true then(
              compile_expr e1;
              Printf.printf "div $a%i,$a%i,%i\n" (!numberOfRegistre-1)(!numberOfRegistre-1) (getTheInt e2);
            )else(
              compile_expr e2;
              Printf.printf "div $a%i,%i,$a%i\n" (!numberOfRegistre-1)(getTheInt e1) (!numberOfRegistre-1);
            )
            )else(
              if(findEInt e2) = true then(
                compile_expr e1;
                Printf.printf "div $a%i,$a%i,%i\n" (!numberOfRegistre-1)(!numberOfRegistre-1) (getTheInt e2);
              )else(
                compile_expr e1;
                compile_expr e2;
                Printf.printf "div $a%i,$a%i,$a%i\n" (!numberOfRegistre-2)(!numberOfRegistre-2) (!numberOfRegistre-1);
                numberOfRegistre := !numberOfRegistre - 1;
              )
            );
;;
let compile_toplevel_expr (e: Ast.expr) : unit =
  Printf.printf ".text\nmain:\n";
  compile_expr e;
  Printf.printf "  li $v0, 1\n  syscall\n  li $v0, 10\n  syscall\n"
