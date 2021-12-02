open ASD

let rec indentation n = match n with
  | n when n <= 0 -> ""
  | n -> "\t" ^ (indentation (n - 1))

and str_of_li_err l err = match l with
  | [] -> ""
  | [x] -> begin match x with
    | IdentExpression s -> s
    | TabptrExpression (s, x) -> s ^ "[" ^ (string_of_int x) ^ "]"
    | _ -> failwith err ^ " d'un objet qui n'est pas une variable"
    end
  | t :: q -> (str_of_li_err [t] err) ^ ", " ^ (str_of_li_err q err)

and prettyprint prog = prettyprint_sta prog 1

and prettyprint_sta sta ind = match sta with
  | AssignStatement (e1, e2) -> (indentation ind) ^ (prettyprint_exp e1) ^ " := " ^ (prettyprint_exp e2) ^ "\n"
  | ProgramStatement l -> (indentation (ind - 1)) ^ "{\n"
                          ^ (String.concat "" (List.map (fun s -> prettyprint_sta s (ind)) l))
                          ^ (indentation (ind - 1)) ^ "}\n"
  | IfStatement (e, s1) -> (indentation ind) ^ "IF " ^ (prettyprint_exp e) ^ " THEN\n"
                           ^ (prettyprint_sta s1 (ind + 1))
                           ^ (indentation ind) ^ "FI\n"
  | IfElseStatement (e, s1, s2) ->    (indentation ind) ^ "IF " ^ (prettyprint_exp e) ^ " THEN\n"
                                    ^ (prettyprint_sta s1 (ind + 1))
                                    ^ (indentation ind) ^ "ELSE\n"
                                    ^ (prettyprint_sta s2 (ind + 1))
                                    ^ (indentation ind) ^ "FI\n"
  | WhileStatement (e, s) -> (indentation ind) ^ "WHILE " ^ (prettyprint_exp e) ^ "\n"
                             ^ (indentation ind) ^ "DO\n"
                             ^ (prettyprint_sta s (ind + 1))
                             ^ (indentation ind) ^ "DONE\n"
  | IntStatement l -> (indentation ind) ^ "INT " ^ (str_of_li_err l "declaration") ^ "\n"
  | ReadStatement l -> (indentation ind) ^ "READ " ^ (str_of_li_err l "lecture") ^ "\n"
  | PrintStatement l -> (indentation ind) ^ "PRINT " ^ (List.fold_right (fun expr concat -> (prettyprint_exp expr ^ (if (concat == "") then "" else ", ") ^ concat)) l "") ^ "\n"

and prettyprint_exp exp =
  match exp with
  | AddExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " + " ^ (prettyprint_exp r) ^ ")"
  | SubExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " - " ^ (prettyprint_exp r) ^ ")"
  | MulExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " * " ^ (prettyprint_exp r) ^ ")"
  | DivExpression (l, r) -> "(" ^ (prettyprint_exp l) ^ " / " ^ (prettyprint_exp r) ^ ")"
  | IntegerExpression i -> string_of_int i
  | StringExpression s -> "\"" ^ s ^ "\""
  | IdentExpression s -> s

(* TODO : extend when you extend the language *)
