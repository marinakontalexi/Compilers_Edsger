open Llvm
open Ast

exception Error of string

let context = global_context ()
let the_module = create_module context "program_module" (*contains all of the functions and global variables*)

(* The Codegen.builder object is a helper object that makes it easy to generate LLVM instructions. 
   Instances of the IRBuilder class keep track of the current place to insert instructions and has methods to create new instructions. *)
let builder = builder context

(* The Codegen.named_values map keeps track of which values are defined in the current scope and what their LLVM representation is. 
   (In other words, it is a symbol table for the code). *)
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

(* types *)
let int_type = i16_type llctx
let char_type = i8_type llctx
let double_type = x86fp80_type llctx
let bool_type = i1_type llctx
let non_type = void_type llctx

let rec codegen_expr (expression:expr) = 
  let name = expr_to_string expression in
  match expression with
  | NULL-> const_null pointer_type int_type

  (* -------------- *)
  (* --- TO SEE --- *)
  (* -------------- *)
  | Id(s) -> (*lookup id; id.value*)
    let v = try Hashtbl.find named_values s with
      | Not_found -> raise (Error "unknown variable name")
    in build_load v s builder (* do we need this? *)

  | True -> const_int bool_type 1
  | False -> const_int bool_type 0
  | INT(i) -> const_int int_type i
  | CHAR(c) -> let ascii = Char.code c in const_int char_type ascii 
  | FLOAT(f) -> const_float double_type f
  | STRING(s) -> 
    let str_init = const_stringz context s in 
    let str = define_global ".str" str_init context in
    let constzero = const_int int_type 0 in
    build_gep str [|constzero|] "strtmp" buidler

  (* -------------- *)
  (* --- TO SEE --- *)
  (* -------------- *)
  | Fun_call(callee, args) -> (* do we need scopes here? *)
    let callee =
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in
    let parameters = params callee in
    if Array.length parameters == Array.length args then () else
      raise (Error "incorrect # arguments passed");
      (* codegen each argument and call the function *)
    let args = Array.map codegen_expr args in
    build_call callee args "calltmp" builder 

  | Table_call(e1, e2) -> ()

  | Un_operation(op, e) ->
    match op with
    | AND -> codegen_expr e
    | POINT -> ()
    | EXC -> ()
    | POS -> codegen_expr e
    | NEG ->
      let v = codegen_expr e1 in
      if ((type_of e1_val) = int_type) then build_neg v "negtmp" builder
      else build_fneg v "negtmp" builder

  | Bin_operation(e1, op, e2) ->
    let e1_val = codegen_expr e1 in
    let e2_val = codegen_expr e2 in
    let val_type = (match classify_type (type_of e1_val) with 
      |Pointer -> element_type (type_of e1_val)
      | _ -> type_of e1_val) in
    match op with
    | PLUS ->
      if (size_of val_type) = (size_of (pointer_type int_type)) then
        build_gep e1_val [|e2_val|] "ptraddtmp" builder
      else if val_type = int_type then build_add e1_val e2_val "addtmp" builder
      else build_fadd e1_val e2_val "addtmp" builder
    | MINUS ->
      if (size_of val_type) = (size_of (pointer_type int_type)) then
        build_gep e1_val [|e2_val|] "ptraddtmp" builder
      else if val_type = int_type then build_sub e1_val e2_val "subtmp" builder
      else build_fsub e1_val e2_val "subtmp" builder
    | TIMES ->
      if ((type_of e1_val) = int_type) then build_mul e1_val e2_val "multmp" builder
      else build_fmul e1_val e2_val "multmp" builder
    | DIV ->
      if ((type_of e1_val) = int_type) then build_sdiv e1_val e2_val "divtmp" builder
      else build_fdiv e1_val e2_val "divtmp" builder
    | MOD ->
      if ((type_of e1_val) = int_type) then build_srem e1_val e2_val "modtmp" builder
      else build_frem e1_val e2_val "modtmp" builder
    | LESS ->
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Slt e1_val e2_val "lesstmp" buidler
      else build_fcmp Fcmp.Olt e1_val e2_val "lesstmp" builder
    | MORE ->
    | LEQ ->
    | GEQ ->
    | EQ ->
    | NEQ ->
    | LOGICAL_AND ->
    | LOGICAL_OR ->
    | COMMA ->()

  | Un_assignment_left(INCR, e) 
  | Un_assignment_right(e, INCR)
  | Un_assignment_left(DECR, e) 
  | Un_assignment_right(e, DECR) -> ()

  | Bin_assignment (e1, ass, e2) ->
    
    match ass with
    | TIMESEQ
    | MODEQ
    | DIVEQ
    | PLUSEQ
    | MINUSEQ
    | ASSIGN -> ()

  | Typecast(ft, e)
  | Question(e1, e2, e3)
  | New(ft, None)
  | New(Type(t, n), Some(e))
  | Delete(e) ->()

