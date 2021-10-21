type token =
    IDENT of string
  | TEXT of string
  | INTEGER of int
  | LP (* parenthesis () *)
  | RP
  | LB (* bracket [] *)
  | RB
  | LC (* cub {} *)
  | RC
  | COM
  | ADD
  | SUB
  | MUL
  | DIV
  | ASSIGN
  | FUNC_KW
  | PROTO_KW
  | INT_KW
  | VOID_KW
  | RETURN_KW
  | PRINT_KW
  | READ_KW
  | IF_KW
  | THEN_KW
  | ELSE_KW
  | FI_KW
  | WHILE_KW
  | DO_KW
  | OD_KW
  | ENDL
