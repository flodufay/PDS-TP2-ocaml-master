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
  | [< s = list_better statement optional_endline; _ = optional_endline >] -> (* print_endline("found program") ; *) ProgramStatement(s)

and parse_endline = parser
  | [< 'ENDL >] -> ()

and optional_endline = parser
  | [< 'ENDL >] -> ()
  | [< >] -> ()

and bloc = parser
  | [< s = list_better statement optional_endline; _ = optional_endline >] -> (* print_endline("found program") ; *) ProgramStatement(s)

(* and func = parser
  | [< 'PROTO_KW ; t = typ ; name = expression ; 'LP ; args = list0 expression comma ; 'RP >] -> FunctionPrototype(t, name, args)
  | [< 'FUNC_KW ; t = typ ; name = expression ; 'LP ; args = list0 expression comma ; 'RP ; ext = function_aux >] -> FunctionDeclaration(t, name, args, ext)

and function_aux = parser
  | [< 'LC ; r = statement ; 'RC >] -> r
  | [< r = statement >] -> r

 and typ = parser
  | [< 'VOID_KW >] -> Type_Void
  | [< 'INT_KW ; t = typ_aux >] -> t

and typ_aux = parser
  | [< 'LB ; size = expression ; 'RB >] -> Type_Array(size)
  | [< >] -> Type_Int *)

and expression = parser
  | [< e1 = factor; e = expression_aux e1 >] -> (* print_endline("found expression") ; *) e

and expression_aux e1 = parser
  | [< 'ADD;  e2 = factor; e = expression_aux (AddExpression (e1, e2)) >] -> (* print_endline("found AddExpression") ; *) e
  | [< 'SUB;  e2 = factor; e = expression_aux (SubExpression (e1, e2)) >] -> e
  | [< 'MUL;  e2 = factor; e = expression_aux (MulExpression (e1, e2)) >] -> e
  | [< 'DIV;  e2 = factor; e = expression_aux (DivExpression (e1, e2)) >] -> e
  | [<>] -> e1

and statement = parser
  | [< 'IF_KW; e = expression; _ = optional_endline; 'THEN_KW; _ = optional_endline; s = statement; _ = optional_endline; res = if_aux e s >] -> (* print_endline("found If Statement") ; *) res
  | [< 'WHILE_KW; e = expression; _ = optional_endline; 'DO_KW; _ = optional_endline; s = statement; _ = optional_endline; 'OD_KW >] -> WhileStatement(e, s)
  | [< 'LC; _ = optional_endline; p = bloc; 'RC >] -> p
  | [< 'INT_KW; l = list0 expression comma >] -> IntStatement(l)
  | [< 'READ_KW; l = list0 expression comma >] -> ReadStatement(l)
  | [< 'PRINT_KW; l = list0 expression comma >] -> PrintStatement(l)
  | [< e1 = expression; s = statement_aux e1 >] -> s

 and if_aux e s = parser
  | [< 'FI_KW >] -> IfStatement (e, s)
  | [< 'ELSE_KW; _ = optional_endline; s2 = statement; _ = optional_endline; 'FI_KW >] -> IfElseStatement (e, s, s2)

and statement_aux e1 = parser
  | [< 'ASSIGN; e2 = expression >] -> (* print_endline("assign") ; *) AssignStatement (e1, e2)

and factor = parser
  | [< e1 = primary; e = factor_aux e1 >] -> e

and factor_aux e1 = parser
  | [< 'LB ; 'INTEGER x ; 'RB >] -> begin match e1 with
      | IdentExpression s -> TabptrExpression (s, x)
      | _ -> failwith "Parsing Error when parsing subscripting to a non-identifier"
  end
  | [< >] -> e1

and primary = parser
  | [< 'INTEGER x >] -> (* print_endline("found integer " ^ (string_of_int x)) ; *) IntegerExpression x
  | [< 'IDENT s >] -> (* print_endline("found variable " ^ s) ; *) IdentExpression s
  | [< 'TEXT s >] -> StringExpression(s)
  (* | [< >] -> FunctionCallExpression(f, args) *)


and comma = parser
  | [< 'COM >] -> ()
