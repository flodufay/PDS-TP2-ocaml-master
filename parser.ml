open ASD
open Token

(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

(* p (sep p)* *)
let rec list1 p sep = parser
  | [< x = p; l = list1_aux p sep >] -> x :: l
and list1_aux p sep = parser
  | [< _ = sep; l = list1 p sep >] -> l
  | [<>] -> []

(* (p (sep p)* )? *)
let list0 p sep = parser
  | [< l = list1 p sep >] -> l
  | [<>] -> []

let rec list_better p sep = parser
  | [< x = p; _ = sep; l = list_better p sep >] -> x :: l
  | [< >] -> []

(* TODO : change when you extend the language *)
let rec program = parser
  | [< s = list_better statement program_endl >] -> ProgramStatement(s)

and program_endl = parser
  | [< 'ENDL >] -> ()

and expression = parser
  | [< e1 = factor; e = expression_aux e1 >] -> e

and expression_aux e1 = parser
  | [< 'ADD;  e2 = factor; e = expression_aux (AddExpression (e1, e2)) >] -> (* print_endline("found AddExpression") ; *) e
  | [< 'SUB;  e2 = factor; e = expression_aux (SubExpression (e1, e2)) >] -> e
  | [< 'MUL;  e2 = factor; e = expression_aux (MulExpression (e1, e2)) >] -> e
  | [< 'DIV;  e2 = factor; e = expression_aux (DivExpression (e1, e2)) >] -> e
  | [<>] -> e1
  (* TODO : that's all? *)

and statement = parser
  | [< e1 = expression; s = statement_aux e1 >] -> s
 
and statement_aux e1 = parser
  | [< 'ASSIGN; e2 = expression >] -> (* print_endline("assign") ; *) AssignStatement (e1, e2)

  (* TODO : that's all? *)

and factor = parser
  | [< e1 = primary; e = factor_aux e1 >] -> e

and factor_aux e1 = parser
  | [<>] -> e1
  (* TODO : that's all? *)

and primary = parser
  | [< 'INTEGER x >] -> (* print_endline("found integer " ^ (string_of_int x)) ; *) IntegerExpression x
  | [< 'IDENT s >] -> (* print_endline("found variable " ^ s) ; *) IdentExpression s

  (* TODO : that's all? *)

and comma = parser
  | [< 'COM >] -> ()
