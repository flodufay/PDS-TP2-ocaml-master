open ASD

let rec prettyprint prog = prettyprint_sta prog

and prettyprint_sta sta = match sta with
  | AssignStatement (e1, e2) -> (prettyprint_exp e1) ^ " := " ^ (prettyprint_exp e2) ^ "\n"
  | ProgramStatement l -> begin match l with
    | t :: q -> (prettyprint t) ^ (prettyprint (ProgramStatement q))
    | [] -> "" end
  | IfStatement (e, s1) -> "IF " ^ (prettyprint_exp e) ^ "\nTHEN\n" ^ (prettyprint_sta s1) ^ "FI\n"
  | IfElseStatement (e, s1, s2) -> "IF " ^ (prettyprint_exp e) ^ "\nTHEN\n" ^ (prettyprint_sta s1) ^ "ELSE\n" ^ (prettyprint_sta s2) ^ "FI\n"

and prettyprint_exp exp =
  match exp with
  | AddExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " + " ^ (prettyprint_exp r) ^ ")"
  | SubExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " - " ^ (prettyprint_exp r) ^ ")"
  | MulExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " * " ^ (prettyprint_exp r) ^ ")"
  | DivExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " / " ^ (prettyprint_exp r) ^ ")"
  | IntegerExpression i -> string_of_int i
  | IdentExpression s -> s

(* TODO : extend when you extend the language *)
