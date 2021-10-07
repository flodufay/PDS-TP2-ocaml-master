(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubstractExpression of expression * expression
  | DivideExpression of expression * expression
  | MultiplyExpression of expression * expression
  | IntegerExpression of int

type typ =
  | Type_Int

type program = expression
