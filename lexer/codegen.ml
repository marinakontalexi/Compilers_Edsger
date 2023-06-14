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
let labels:(string, {llbasicblock; llbasicblock}) Hash_table.t = Hash_table.create 10

(* types *)
let int_type = i16_type llctx
let char_type = i8_type llctx
let double_type = x86fp80_type llctx
let bool_type = i1_type llctx
let non_type = void_type llctx

let rec codegen_decl (decl : declaration) = 
  match decl with
  | Var_declaration(ft, dl)
  | Fun_declaration(ft,id,pl)
  | Fun_definition(ft,id,pl,dl,sl) -> ()

let rec codegen_stmt (statement:stmt) = 
  match statement with
  | Empty_stmt -> ()
  | Expression(e) -> ignore (codegen_expr e)
  | Stmt_block(sl) -> ignore (List.map codegen_stmt sl)
  | If(e,s_then, s_else) ->
      let cond = codegen_expr e in
      let zero = const_int bool_type 0 in
      let cond_val = build_icmp Icmp.Ne cond zero "ifcond" builder in
      let start_bb = insertion_block builder in
      let f = block_parent start_bb in
      let then_bb = append_block context "then" f in
      let else_bb = append_block context "else" f in
      let end_if = append_block context "endif" f in
      ignore (build_cond_br cond_val then_bb else_bb builder);

      position_at_the_end then_bb buidler;
      ignore (codegen_stmt s_then);
      ignore (build_br end_if builder);
      
      position_at_end else_bb builder;
      ignore(match s_else with
      | Some s -> codegen_stmt s
      | None -> ());
      ignore (build_br end_if builder);

      ignore (position_at_end end_if builder)

  | For(label, init, cond, step, s) ->
    ignore (match init with
      | Some(e) -> codegen_expr e 
      | None -> () in);
    let prev_bb = insertion_block builder in
    let f = block_parent prev_bb in
    let loopcond_bb = append_block context "loopcond" in
    let loopbody_bb = append_block context "loopbody" f in
    let loopstep_bb = append_block context "loopstep" f in
    let endfor_bb = append_block context "endfor" f in
    ignore (match label with
      (* Add label to labels *)
      | Some(id) -> Hashtbl.add labels id {cont: loopstep_bb; break: endfor_bb}
      | None -> Hashtbl.add labels "currentFor" {cont: loopstep_bb; break: endfor_bb});
    ignore (build_br loopcond_bb builder);

    position_at_end loopcond_bb builder;
    let end_cond = match cond with
      | Some(e) -> codegen_expr e
      | None -> () in
    let zero = const_int bool_type 0 in
    let cond_val = build_icmp Icmp.Ne cond zero "forendcond" builder in
    ignore (build_cond_br end_cond loopbody_bb endfor_bb builder);

    position_at_end loopbody_bb builder;
    ignore (codegen_stmt s);
    ignore (build_br loopstep_bb builder);

    position_at_end loopstep_bb builder;
    ignore (match step with
      | Some(e) -> codegen_expr e2
      | None -> ()); 
    ignore (build_br loopcond_bb builder);
    
    ignore (position_at_end endfor_bb builder);

    ignore (match label with 
      | Some(l) -> Hashtbl.remove labels l
      | None -> Hashtbl.remove labels "currentFor")

  | Continue(label) ->
    let jumpLoop = match label with
      | Some(l) -> Hashtbl.find labels l 
      | None -> Hashtbl.find lables "currentFor" in
    ignore (build_br jumpLoop.cont builder)
  | Break(label) ->
    let jumpLoop = match label with
      | Some(l) -> Hashtbl.find labels l 
      | None -> Hashtbl.find lables "currentFor" in
    ignore (build_br jumpLoop.break builder)
  | Return(e) -> 
    ignore (match e with
      | Some(ret) -> 
          let vl = codegen_expr ret in
          build_ret vl buidler 
      | None -> build_ret_void buidler)

let rec codegen_expr (expression:expr) = 
  let name = expr_to_string expression in
  match expression with
  | NULL-> const_null pointer_type int_type

  (* -------------- *)
  (* --- TO SEE --- *)
  (* -------------- *)
  (* alloca *)
  | Id(s) -> (*lookup id; id.value*)
    let v = try Hashtbl.find named_values s with
      | Not_found -> raise (Error "unknown variable name")
    in build_load v s builder 

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

  | Table_call(e1, e2) -> 
    (* we need ptr or the deref of ptr? *)
    let ptr = codegen_expr e1 in
    let offset = codegen_expr e2 in
    build_gep ptr [|offset|] "tableCalltmp" builder

  | Un_operation(op, e) ->
    let vl = codegen_expr e
    match op with
    | AND -> vl
    | POINT -> (*do we need smt else?*)
      build_load vl "loadtmp" buidler
    | EXC -> build_not vl "nottmp" builder
    | POS -> vl
    | NEG -> 
      if ((type_of vl) = int_type) then build_neg vl "negtmp" builder
      else build_fneg vl "negtmp" builder

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
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Sgt e1_val e2_val "moretmp" buidler
      else build_fcmp Fcmp.Ogt e1_val e2_val "moretmp" builder
    | LEQ ->
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Sle e1_val e2_val "leqtmp" buidler
      else build_fcmp Fcmp.Oge e1_val e2_val "leqtmp" builder
    | GEQ ->
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Sge e1_val e2_val "geqtmp" buidler
      else build_fcmp Fcmp.Oge e1_val e2_val "geqtmp" builder
    | EQ ->
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Eq e1_val e2_val "eqtmp" buidler
      else build_fcmp Fcmp.Oeq e1_val e2_val "eqtmp" builder
    | NEQ ->
      if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
        build_icmp Icmp.Ne e1_val e2_val "neqtmp" buidler
      else build_fcmp Fcmp.One e1_val e2_val "neqtmp" builder
    | LOGICAL_AND ->
    | LOGICAL_OR ->
    | COMMA ->()

    (* SOMETHING SHOULD BE DIFFERENT BASED ON POSITION OF e *)
  | Un_assignment_left(INCR, e) 
  | Un_assignment_right(e, INCR)-> 
    let e_val = codegen_expr e
    let vl = codegen_expr Bin_operation(e, PLUS, INT(1)) in
    build_store vl e_val builder
  | Un_assignment_left(DECR, e) 
  | Un_assignment_right(e, DECR) -> 
    let e_val = codegen_expr e
    let vl = codegen_expr Bin_operation(e, MINUS, INT(1)) in
    build_store vl e_val builder

  | Bin_assignment (e1, ass, e2) ->
    let e1_val = codegen_expr e1 in
    let vl = match ass with
      | TIMESEQ -> codegen_expr Bin_operation(e1, TIMES, e2)
      | MODEQ  -> codegen_expr Bin_operation(e1, MOD, e2)
      | DIVEQ -> codegen_expr Bin_operation(e1, DIV, e2)
      | PLUSEQ -> codegen_expr Bin_operation(e1, PLUS, e2)
      | MINUSEQ -> codegen_expr Bin_operation(e1, MINUS, e2)
      | ASSIGN -> codegen_expr e2
    in build_store vl e1_val buidler


  | Typecast(ft, e)
  | Question(e1, e2, e3)
  | New(ft, None)
  | New(Type(t, n), Some(e))
  | Delete(e) ->()