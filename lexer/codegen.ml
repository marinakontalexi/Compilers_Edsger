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

let rec codegen_stmt (statement:stmt) = 
  match statement with
  | Empty_stmt -> ()
  | Expression(e) -> codegen_expr e
  | Stmt_block(sl) -> List.map codegen_stmt sl

  | If(e,s_then, s_else) ->
      let cond = codegen_expr e in
      let zero = const_int bool_type 0 in
      let cond_val = build_icmp Icmp.Ne cond zero "ifcond" builder in
      (* saving a pointer to the first block which we’ll need to build a conditional branch later *)
      let start_bb = insertion_block builder in
      (* f = current Function object that is being built *)
      let f = block_parent start_bb in
      let then_bb = append_block context "then" f in
      
      (* since the “then” block is empty, it also starts out by inserting at the beginning of the block *)
      position_at_the_end then_bb buidler;
      let then_val = codegen_stmt s_then in
      (* Codegen of 'then' can change the current block, update then_bb for the
      * phi. We create a new name because one is used for the phi node, and the
      * other is used for the conditional branch. *)
      let new_then_bb = insertion_block builder in

      let else_bb = append_block context "else" the_function in
      position_at_end else_bb builder;
      let else_val = match s_else with
      | Some s -> codegen_stmt s
      | None -> () in
      let new_else_bb = insertion_block builder in

      let merge_bb = append_block context "ifcont" the_function in
      position_at_end merge_bb builder;
      (* create the PHI node and set up the block/value pairs for the PHI *)
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = build_phi incoming "iftmp" builder in

      (* Return to the start block to add the conditional branch. *)
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);

      (* Note that creating new blocks does not implicitly affect the IRBuilder, 
       * so it is still inserting into the block that the condition went into.*)

      (* Set a unconditional branch at the end of the 'then' block and the
      * 'else' block to the 'merge' block. *)
      position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

      (* Finally, set the builder to the end of the merge block. *)
      position_at_end merge_bb builder;

      phi

  | For(label, init, cond, step, s) ->
    let start_val = match init with
      | Some(e) -> codegen_expr e 
      | None -> () in
      
    let prev_bb = insertion_block builder in
    let f = block_parent prev_bb in
    let loopHeader_bb = append_block context "loopHeader_bb" in
    let loop_bb = append_block context "loop" f in
    let step_bb = append_block context "loopstep" f in
    let after_bb = append_block context "afterloop" f in
    ignore (match label with
      (* Add label to labels *)
      | Some(id) -> Hashtbl.add labels id {cont: step_bb; break: after_bb}
      | None -> Hashtbl.add labels "currentFor" {step_bb; after_bb});
    ignore (build_br loopHeader_bb builder);

    position_at_end loopHeader_bb builder;
    let end_cond = match cond with
      | Some(e) -> codegen_expr e
      | None -> () in
    let zero = const_int bool_type 0 in
    let cond_val = build_icmp Icmp.Ne cond zero "forendcond" builder in
    ignore (build_cond_br end_cond loop_bb after_bb builder);

    position_at_end loop_bb builder;
    (* do we need to add i in Hash_table? *)
    let variable = build_phi [(start_val, loopHeader_bb)] "i" builder in
    ignore (codegen_stmt s);
    ignore (build_br step_bb builder);

    position_at_end step_bb builder;
    let step_val = match step with
      | Some(e) -> codegen_expr e
      | None -> () (* Default value? *)
    let next_var = build_add variable step_val "nextvar" builder in
    let loop_end_bb = insertion_block builder in (* this is used for PHI node *)
    ignore (build_br loopHeader_bb builder);
    
    position_at_end after_bb builder;
    add_incoming (next_var, loop_end_bb) variable;
    
    ignore (match label with 
      | Some(l) -> ()
      | None -> Hashtbl.remove labels "currentFor")

    (* ti epistrefei to for? *)

  | Continue(label) ->
    let jumpLoop = match label with
      | Some(l) -> Hashtbl.find labels l 
      | None -> Hashtbl.find lables "currentFor" in
    ignore (build_br jumpLoop.cont builder)
  | Break(id) ->
    let jumpLoop = match label with
      | Some(l) -> Hashtbl.find labels l 
      | None -> Hashtbl.find lables "currentFor" in
    ignore (build_br jumpLoop.break builder)
  | Return(e) -> (* theloume na epistrefei kati?  mipos na valoume ignore? *) 
    match e with
      | Some(ret) -> 
          let vl = codegen_expr ret in
          build_ret vl buidler 
      | None -> build_ret_void buidler

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

