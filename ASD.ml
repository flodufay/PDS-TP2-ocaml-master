(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | DivExpression of expression * expression
  | MulExpression of expression * expression
  | IntegerExpression of int
  | IdentExpression of string
  | TabptrExpression of string * int
  | StringExpression of string
  (* | FunctionCallExpression of expression * expression list *)

type statement =
  | AssignStatement of expression * expression
  | ProgramStatement of statement list
  | IfStatement of expression * statement
  | IfElseStatement of expression * statement * statement
  | WhileStatement of expression * statement
  | IntStatement of expression list
  | ReadStatement of expression list
  | PrintStatement of expression list

type typ =
  | Type_Int
  | Type_Array of int

(* type func =
  | FunctionPrototype of typ * expression * expression list
  | FunctionDeclaration of typ * expression * expression list * statement *)

type program = statement
