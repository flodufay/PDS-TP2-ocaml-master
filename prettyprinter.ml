open ASD

let rec prettyprint prog = prettyprint_sta prog

and prettyprint_sta sta = match sta with
  | AssignStatement (e1, e2) -> (prettyprint_exp e1) ^ " := " ^ (prettyprint_exp e2) ^ "\n"
  | ProgramStatement l -> match l with
    | t :: q -> (prettyprint t) ^ (prettyprint (ProgramStatement q))
    | [] -> ""

and prettyprint_exp exp =
  match exp with
  | AddExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " + " ^ (prettyprint_exp r) ^ ")"
  | SubExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " - " ^ (prettyprint_exp r) ^ ")"
  | MulExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " * " ^ (prettyprint_exp r) ^ ")"
  | DivExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " / " ^ (prettyprint_exp r) ^ ")"
  | IntegerExpression i -> string_of_int i
  | IdentExpression s -> s

(* TODO : extend when you extend the language *)
