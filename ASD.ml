(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | DivExpression of expression * expression
  | MulExpression of expression * expression
  | IntegerExpression of int
  | IdentExpression of string
  | AssignExpression of expression * expression

type typ =
  | Type_Int

type program = expression
