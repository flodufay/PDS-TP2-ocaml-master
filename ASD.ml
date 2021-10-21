(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | DivExpression of expression * expression
  | MulExpression of expression * expression
  | IntegerExpression of int
  | IdentExpression of string
  
type statement =
  | AssignStatement of expression * expression
  | ProgramStatement of statement list
  | IfStatement of expression * statement
  | IfElseStatement of expression * statement * statement

type typ =
  | Type_Int

type program = statement
