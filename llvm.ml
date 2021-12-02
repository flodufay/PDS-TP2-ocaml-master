open Utils
open SymbolTable
(* TODO : extend when you extend the language *)

(* This file contains a simple LLVM IR representation *)
(* and methods to generate its string representation  *)

type llvm_type =
  | LLVM_type_i32
  | LLVM_type_i32_pointeur
(* TODO: to complete *)

type llvm_var = string

type llvm_value =
  | LLVM_i32 of int
  | LLVM_var of llvm_var
  | LLVM_tab_var of llvm_var * llvm_value
(* TODO: to complete? *)


type llvm_ir = (* type of generated IR *)
  { header: llvm_instr_seq; (* instructions to be placed before all code (global definitions) *)
    body: llvm_instr_seq;
  }

 and llvm_instr_seq = (* type of sequences of instructions *)
   | Empty
   | Atom of llvm_instr
   | Concat of llvm_instr_seq * llvm_instr_seq

 and llvm_instr = string (* type of instructions *)

let fmtRead = "@.fmtRead = global [3 x i8 ] c\"%d\\" ^ "00\" "
let fmtIdent = "@.fmtIdent = global [3 x i8 ] c\"%d\\00\""
(* empty IR *)
let empty_ir = {
  header = Empty;
  body = Empty;
}

(* appending an instruction in the header: ir @^ i *)
let (@^) ir i = {
    header = Concat (ir.header, Atom i);
    body = ir.body;
  }

(* appending an instruction in the body: ir @: i *)
let (@:) ir i = {
    header = ir.header;
    body = Concat (ir.body, Atom i);
  }

(* concatenation of two IRs: ir1 @@ ir2 *)
let (@@) ir1 ir2 = {
    header = Concat (ir1.header, ir2.header);
    body = Concat (ir1.body, ir2.body);
}

(* actual IR generation *)
let rec string_of_type = function
  | LLVM_type_i32 -> "i32"
  | LLVM_type_i32_pointeur -> "i32*"

and string_of_var x = x

and string_of_label x = String.sub x 1 (String.length x - 1)

and string_of_value = function
  | LLVM_i32 n -> string_of_int n
  | LLVM_var x -> string_of_var x
  | LLVM_tab_var (x, i) -> string_of_var  "essaye d'écrire t[i]"

and string_of_ir ir =
  (* this header describe to LLVM the target
   * and declare the external function printf
   *)
  "; Target\n"
  ^ "target triple = \"x86_64-unknown-linux-gnu\"\n"
  ^ "; External declaration of the printf function\n"
  ^ "declare i32 @printf(i8* noalias nocapture, ...)\n"
  ^ "declare i32 @scanf(i8* noalias nocapture, ...)\n"
  ^ "\n; Actual code begins\n"
  ^ string_of_instr_seq ir.header
  ^ fmtRead ^ "\n" ^ fmtIdent ^ "\n\n"
  ^ string_of_instr_seq ir.body

and string_of_instr_seq = function
  | Empty -> ""
  | Atom i -> i
  | Concat (li1,li2) -> string_of_instr_seq li1 ^ string_of_instr_seq li2

and string_of_instr i = i


(* functions for the creation of various instructions *)
let rec llvm_load ~(var : llvm_value) : llvm_value * llvm_instr =
  match var with
    |LLVM_var y -> let x = LLVM_var (newtmp ()) in
      (x,string_of_value x ^ " = load i32, i32* " ^ string_of_value var ^ "\n")
    |LLVM_tab_var (y, i) -> begin
        let v1, s1 = llvm_load ~var:i in
        let size = lookup_size !sym_tab y in
        let x1 = newtmp() in
        let x2 = newtmp() in
        (x2, s1 ^ (string_of_var x1) ^ " = getelementptr ["^ (string_of_int size) ^" x i32], ["^ (string_of_int size) ^" x i32]* " ^ (string_of_var y) ^ ", i64 0, i32 " ^ (string_of_value v1) ^ "\n" ^ (string_of_var x2) ^ " = load i32, i32* " (string_of_value x1) "\n")
    end
    |_ -> (var,"")

let llvm_add ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  let v1,s1 = llvm_load ~var:left in
  let v2,s2 = llvm_load ~var:right in
  s1 ^ s2 ^ string_of_var res_var ^ " = add " ^ string_of_type res_type ^ " " ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ "\n"

let llvm_minus ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  let v1,s1 = llvm_load ~var:left in
  let v2,s2 = llvm_load ~var:right in
  s1 ^ s2 ^ string_of_var res_var ^ " = sub " ^ string_of_type res_type ^ " " ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ "\n"

let llvm_mul ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  let v1,s1 = llvm_load ~var:left in
  let v2,s2 = llvm_load ~var:right in
  s1 ^ s2 ^ string_of_var res_var ^ " = mul " ^ string_of_type res_type ^ " " ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ "\n"

let llvm_div ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  let v1,s1 = llvm_load ~var:left in
  let v2,s2 = llvm_load ~var:right in
  s1 ^ s2 ^ string_of_var res_var ^ " = udiv " ^ string_of_type res_type ^ " " ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ "\n"

let llvm_assign ~(res_var : llvm_value) ~(res_type : llvm_type) ~(start_type : llvm_type) ~(right : llvm_value) : llvm_instr =
  let r_v, r_s = llvm_load right in
  match res_var with
<<<<<<< HEAD
  |LLVM_var x -> "store " ^ string_of_type start_type ^ " " ^ string_of_value r_v ^ ", " ^ string_of_type res_type ^ " " ^ string_of_var x ^ "\n"
  |LLVM_tab_var (x, i) ->   let v, s = llvm_load ~var:i in
  let size = lookup_size !sym_tab x in
  let ptr = newtmp() in
  ptr ^ " = getelementptr [" ^ string_of_int size ^ " x i32 ] , [" ^ string_of_int size ^ " x i32 ]∗ " ^ x ^ ", i64 0 , i32" ^ v ^ "\n" ^
  "store " ^ string_of_type start_type ^ " " ^ string_of_value r_v ^ ", " ^ string_of_type res_type ^ " " ^ ptr ^ "\n"
=======
  | LLVM_i32 x -> failwith "assignation à un entier"
  | LLVM_var x -> "store " ^ string_of_type start_type ^ " " ^ (string_of_value r_v) ^ ", " ^ string_of_type res_type ^ " " ^ (string_of_var x) ^ "\n"
  | LLVM_tab_var (x, i) ->   let v, s = llvm_load ~var:i in
  let size = lookup_size sym_tab x in
  let ptr = newtmp() in
  ptr ^ " = getelementptr [" ^ string_of_int size ^ " x i32 ] , [" ^ string_of_int size ^ " x i32 ]∗ " ^ x ^ ", i64 0 , i32" ^ (string_of_value v) ^ "\n" ^
  "store " ^ string_of_type start_type ^ " " ^ (string_of_value r_v) ^ ", " ^ string_of_type res_type ^ " " ^ ptr ^ "\n"
>>>>>>> 285a0f2f589a7a5176403d9e2cb3498c6a1c1e23
  (*
currently produces (for example)
  %v = 0
should produce
  store i32 0, i32* %v
*)

let llvm_return ~(ret_type : llvm_type) ~(ret_value : llvm_value) : llvm_instr =
  let v,s = llvm_load ~var:ret_value in
  s ^ "ret " ^ string_of_type ret_type ^ " " ^ string_of_value v ^ "\n"

let llvm_cmp ~(bool_var : llvm_var) ~(cmp_val : llvm_value) : llvm_instr =
  let v,s = llvm_load ~var:cmp_val in
  s ^ string_of_var bool_var ^ " = " ^ "icmp ne i32 " ^ string_of_value v ^ " , 0 " ^ "\n"

let llvm_goToIf ~(bool_val : llvm_value) ~(then_var : llvm_var) ~(fi_var : llvm_var) : llvm_instr =
  "br i1 " ^ string_of_value bool_val ^ " , " ^ "label " ^ string_of_var then_var ^ " , label " ^ string_of_var fi_var ^ "\n"

let llvm_goToThen ~(fi_var : llvm_var) : llvm_instr =
  "br label " ^ string_of_var fi_var ^ "\n"

let llvm_read ~(var : llvm_value) : llvm_instr =
  "call i32 (i8* , ...) @scanf ( i8* getelementptr inbounds ([3 x i8 ] , [3 x i8 ]*" ^ "@.fmtRead" ^ " ,
  i64 0 , i64 0) , i32* " ^ string_of_value(var) ^ ") \n"

let llvm_print ~(var : string) : llvm_ir =
  let res, len = string_transform var in
  let fmt = newglob "print" in
  let fmt2 =  fmt ^ " = global [" ^ string_of_int len ^ " x i8 ] c\"" ^ res ^ "\"" in
  let body = "call i32 (i8*, ...) @printf ( i8* getelementptr inbounds ([" ^ string_of_int (len) ^ " x i8 ] , [" ^ string_of_int (len) ^ " x i8 ]* " ^ fmt ^ " ,
  i64 0 , i64 0) ) \n" in
  {header = Atom fmt2 ; body = Atom body}

let llvm_print_ident ~(var : llvm_value) : llvm_instr =
  let v1,s1 = llvm_load ~var:var in
  s1 ^ "call i32 (i8*, ...) @printf ( i8* getelementptr inbounds ([3 x i8 ] , [3 x i8 ]* " ^ "@.fmtIdent" ^ " ,
  i64 0 , i64 0), i32 " ^ string_of_value v1 ^ " ) \n"

(* defining the 'main' function with ir.body as function body *)
let llvm_define_main (ir : llvm_ir) : llvm_ir =
  { header = ir.header;
    body = Atom ("define i32 @main() {\n" ^ string_of_instr_seq ir.body ^ "}\n");
  }

(* TODO: complete with other LLVM instructions *)
