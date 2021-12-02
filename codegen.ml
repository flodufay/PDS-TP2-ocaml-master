open ASD
open Llvm
open Utils
open SymbolTable


(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : program) : llvm_ir = (* TODO: change 'expression' when you extend the language *)
  (* TODO : change when you extend the language *)
  let ir, v = ir_of_statement prog in
  (* adds the return instruction *)
  let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in
  (* We create the function main *)
  let ir = llvm_define_main ir in
  ir

(* translation from VSL+ types to LLVM types *)
and llvm_type_of_asd_typ : typ -> llvm_type = function
  | Type_Int -> LLVM_type_i32

(* all expressions have type LLVM_type_i32 *)
(* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression : expression -> llvm_ir * llvm_value = function
  | IntegerExpression i ->
     empty_ir, LLVM_i32 i
  | AddExpression (e1,e2) ->
     let ir1, v1 = ir_of_expression e1 in
     let ir2, v2 = ir_of_expression e2 in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | SubExpression (e1,e2) ->
     let ir1, v1 = ir_of_expression e1 in
     let ir2, v2 = ir_of_expression e2 in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_minus ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | MulExpression (e1,e2) ->
     let ir1, v1 = ir_of_expression e1 in
     let ir2, v2 = ir_of_expression e2 in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | DivExpression (e1,e2) ->
     let ir1, v1 = ir_of_expression e1 in
     let ir2, v2 = ir_of_expression e2 in
     let x = newtmp () in
     let ir = ir1 @@ ir2 @: llvm_div ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
     ir, LLVM_var x
  | IdentExpression s ->
     empty_ir, LLVM_var ("%v" ^ s)
  | TabptrExpression (s, i) ->
     empty_ir, LLVM_tab_var ("%v" ^ s, LLVM_i32 i)


and ir_of_statement : statement -> llvm_ir * llvm_value = function
   |IntStatement(l) -> begin match l with
      |IdentExpression(s)::q -> let ir, v = ir_of_statement (IntStatement(q)) in
   ((((empty_ir) @:"%v" ^ s ) @: " = alloca i32\n" )@@ ir), v
      |TabptrExpression(s, i)::q -> let ir, v = ir_of_statement (IntStatement(q)) in
      add sym_tab (VariableSymbol(Type_Array(i), "%v" ^ s));
   ((((empty_ir) @:"%v" ^ s ) @: " = alloca [" ^ string_of_int i ^ " x i32]\n" )@@ ir), v
      |[] -> empty_ir, (LLVM_i32 0)
      |_ -> failwith("déclaration d'un objet qui n'est pas une variable")
   end

   | AssignStatement (e1, e2) ->
      let ir1, v1 = ir_of_expression e1 in
      let ir2, v2 = ir_of_expression e2 in
      let ir = ir2 @@ ( ir1 @: llvm_assign ~res_var:v1 ~res_type:LLVM_type_i32_pointeur ~start_type:LLVM_type_i32 ~right:v2 ) in
      ir, v1

   | ProgramStatement (l) -> let rec program_statement_aux l res var =
      match l with
         | t::q -> let ir, v = ir_of_statement t in
            program_statement_aux q ( res @@ ir ) v
         | _ -> res, var
      in program_statement_aux l empty_ir (LLVM_i32 0)

   | IfStatement(e, s) ->
      let ir1, v1 = ir_of_expression e in
      let ir2, v2 = ir_of_statement s in
      let x = newtmp() in
      let tmpThen, tmpFi = newlab("then"), newlab("fi") in
      let  ir = (((((((((((empty_ir @: llvm_cmp x v1) @: llvm_goToIf (LLVM_var x) tmpThen tmpFi) @: "\n" )@: string_of_label tmpThen) @: ":\n") @@ ir2) @: "\n")@: llvm_goToThen tmpFi )@: "\n" )@: string_of_label tmpFi )@: ":\n\n") in
      ir, v2

   | IfElseStatement(e, s1, s2) ->
      let ir1, v1 = ir_of_expression e in
      let ir2, v2 = ir_of_statement s1 in
      let ir3, v3 = ir_of_statement s2 in
      let x = newtmp() in
      let tmpThen, tmpElse, tmpFi = newlab("then"), newlab("else"), newlab("fi") in
      let  ir = (((((((((((((((((empty_ir @: llvm_cmp x v1) @: llvm_goToIf (LLVM_var x) tmpThen tmpElse) @: "\n" )@: string_of_label tmpThen) @: ":\n") @@ ir2) @: "\n") @: llvm_goToThen tmpFi )@: "\n" )@: string_of_label tmpElse) @: ":\n") @@ ir3) @: "\n")@: llvm_goToThen tmpFi )@: "\n" )@: string_of_label tmpFi )@: ":\n\n") in
      ir, v2

   | WhileStatement(e, s) ->
      let ir1, v1 = ir_of_expression e in
      let ir2, v2 = ir_of_statement s in
      let x = newtmp() in
      let tmpDo, tmpDone = newlab("do"), newlab("done") in
      let  ir = (((((((((((((empty_ir @: llvm_cmp x v1) @: llvm_goToIf (LLVM_var x) tmpDo tmpDone) @: "\n" )@: string_of_label tmpDo) @: ":\n") @@ ir2) @: "\n" )@: llvm_goToIf (LLVM_var x) tmpDo tmpDone) @: "\n" )@: llvm_goToThen tmpDone )@: "\n" )@: string_of_label tmpDone )@: ":\n\n") in
      ir, v2

   | ReadStatement(l) -> begin match l with
      | IdentExpression(s)::q -> let ir, v = ir_of_statement(ReadStatement(q)) in
         ((empty_ir @: (llvm_read(LLVM_var("%v" ^ s)))) @@ ir), v
      | [] -> empty_ir, (LLVM_i32 0)
      | _ -> failwith("read un objet qui n'est pas une variable")
   end


   | PrintStatement(l) -> begin match l with
      | StringExpression(s)::q -> let ir, v = ir_of_statement(PrintStatement(q)) in
         ((empty_ir @@ (llvm_print(s))) @@ ir), v
      | IdentExpression(s)::q -> let ir1, v1 = ir_of_statement(PrintStatement(q)) in
                                 let ir2,v2 = ir_of_expression(IdentExpression(s)) in
         ((empty_ir @: (llvm_print_ident(v2))) @@ ir1), v1
      | [] -> empty_ir, (LLVM_i32 0)
      | _ -> failwith("print un objet qui n'est pas une string")
   end

(* TODO: complete with new cases and functions when you extend your language *)
