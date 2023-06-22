open Llvm
open Ast
open Address_records
open Bool

exception Error of string

let exist_ret_stmt = ref false
let context = global_context ()
let the_module = create_module context "program_module"
let builder = builder context

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
type labeltype = {mutable cont: llbasicblock; mutable break: llbasicblock}
let labels:(string, labeltype) Hashtbl.t = Hashtbl.create 10

(* types *)
let int_type = i16_type context
let char_type = i8_type context
let double_type = x86fp80_type context
let bool_type = i1_type context
let non_type = void_type context

(* alloca functions *)
let create_entry_block_alloca the_function ft var_name =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca ft var_name builder

let rec ft_to_llvmtype ft =
  match ft with
  | Type(Int, 0) -> int_type
  | Type(Char, 0) -> char_type
  | Type(Double, 0) -> double_type
  | Type(Bool, 0) -> bool_type
  | Type(t, n) ->
    match t with
    Int | Char | Double | Bool -> pointer_type (ft_to_llvmtype (Type(t, n-1)))
  | _ -> non_type

let create_argument_allocas the_function pl =
  let f_help (Param(_,ft,Id(s))) =  (ft_to_llvmtype ft, s) in
  let args = Array.of_list (List.map f_help pl) in
  Array.iteri (fun i ai ->
    let (ft, var_name) = args.(i) in
    let alloca = create_entry_block_alloca the_function ft var_name in
    (* Store the initial value into the alloca. *)
    ignore(build_store ai alloca builder);

    (* Add arguments to variable symbol table. *)
    (* Hashtbl.add named_values var_name alloca; *)
    ignore(Address_records.variable_push var_name alloca);
  ) (params the_function)

  let rec codegen_expr (expression:expr) = 
    let name = expr_to_string expression in
    match expression with
    | NULL-> const_null (pointer_type int_type)
  
    | Id(s) -> 
      let Var(_, alloca, _) = variable_find s
      in build_load alloca s builder 
    (* in alloca *)
  
    | True -> const_int bool_type 1
    | False -> const_int bool_type 0
    | INT(i) -> const_int int_type i
    | CHAR(c) -> let ascii = Char.code c in const_int char_type ascii 
    | FLOAT(f) -> const_float double_type f
    | STRING(s) -> 
      let str_init = const_stringz context s in 
      let str = define_global ".str" str_init the_module in
      let constzero = const_int int_type 0 in
      build_gep str [|constzero|] "strtmp" builder
  
    (* -------------- *)
    (* --- TO SEE --- *)
    (* -------------- *)
    | Fun_call(Id(callee), args) -> 
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let args = Array.of_list (List.map codegen_expr args) in
      build_call callee args "calltmp" builder 
  
    | Table_call(e1, e2) -> 
      (* we need ptr or the deref of ptr? *)
      let ptr = codegen_expr e1 in
      let offset = codegen_expr e2 in
      build_gep ptr [|offset|] "tableCalltmp" builder
  
    | Un_operation(op, e) ->
      (let vl = codegen_expr e in
      match op with
      | AND -> vl
      | POINT -> (*do we need smt else?*)
        build_load vl "loadtmp" builder
      | EXC -> build_not vl "nottmp" builder
      | POS -> vl
      | NEG -> 
        if ((type_of vl) = int_type) then build_neg vl "negtmp" builder
        else build_fneg vl "negtmp" builder)
  
    | Bin_operation(e1, op, e2) ->(
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
          build_icmp Icmp.Slt e1_val e2_val "lesstmp" builder
        else build_fcmp Fcmp.Olt e1_val e2_val "lesstmp" builder
      | MORE ->
        if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sgt e1_val e2_val "moretmp" builder
        else build_fcmp Fcmp.Ogt e1_val e2_val "moretmp" builder
      | LEQ ->
        if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sle e1_val e2_val "leqtmp" builder
        else build_fcmp Fcmp.Oge e1_val e2_val "leqtmp" builder
      | GEQ ->
        if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sge e1_val e2_val "geqtmp" builder
        else build_fcmp Fcmp.Oge e1_val e2_val "geqtmp" builder
      | EQ ->
        if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Eq e1_val e2_val "eqtmp" builder
        else build_fcmp Fcmp.Oeq e1_val e2_val "eqtmp" builder
      | NEQ ->
        if (val_type = int_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Ne e1_val e2_val "neqtmp" builder
        else build_fcmp Fcmp.One e1_val e2_val "neqtmp" builder

      (* | LOGICAL_AND 
      | LOGICAL_OR 
      | COMMA ->(* NOT WRITE CODE*)  *)
      )
  
      (* SOMETHING SHOULD BE DIFFERENT BASED ON POSITION OF e *)
    | Un_assignment_left(INCR, e) 
    | Un_assignment_right(e, INCR)-> 
      let e_val = match e with 
      | Id(s) -> let Var(_, ret, _) = variable_find s in ret
      | _ -> codegen_expr e in 
      let vl = codegen_expr (Bin_operation(e, PLUS, INT(1))) in
      build_store vl e_val builder
    | Un_assignment_left(DECR, e) 
    | Un_assignment_right(e, DECR) -> 
      let e_val = match e with 
      | Id(s) -> let Var(_, ret, _) = variable_find s in ret
      | _ -> codegen_expr e in 
      let vl = codegen_expr (Bin_operation(e, MINUS, INT(1))) in
      build_store vl e_val builder
  
    | Bin_assignment (e1, ass, e2) ->
      let e1_val = match e1 with 
      | Id(s) -> let Var(_, ret, _) = variable_find s in ret
      | _ -> codegen_expr e1 in 

      let vl = match ass with
        | TIMESEQ -> codegen_expr (Bin_operation(e1, TIMES, e2))
        | MODEQ  -> codegen_expr (Bin_operation(e1, MOD, e2))
        | DIVEQ -> codegen_expr (Bin_operation(e1, DIV, e2))
        | PLUSEQ -> codegen_expr (Bin_operation(e1, PLUS, e2))
        | MINUSEQ -> codegen_expr (Bin_operation(e1, MINUS, e2))
        | ASSIGN -> codegen_expr e2(* 
          let rval = match e2 with
        | Id(s) -> build_load (codegen_expr e2) s builder
        | _ -> codegen_expr e2
        *)
      in build_store vl e1_val builder
  
  
    (* | Typecast(ft, e)->()
    | Question(e1, e2, e3)->()
    | New(ft, None)->()
    | New(Type(t, n), Some(e))->()
    | Delete(e) ->() *)

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

    position_at_end then_bb builder;
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
    | Some(e) -> ignore(codegen_expr e)
    | None -> ());
  let prev_bb = insertion_block builder in
  let f = block_parent prev_bb in
  let loopcond_bb = append_block context "loopcond" f in
  let loopbody_bb = append_block context "loopbody" f in
  let loopstep_bb = append_block context "loopstep" f in
  let endfor_bb = append_block context "endfor" f in
  ignore (match label with
    (* Add label to labels *)
    | Some(Id(id)) -> Hashtbl.add labels id {cont = loopstep_bb; break = endfor_bb}
    | None -> Hashtbl.add labels "currentFor" {cont = loopstep_bb; break = endfor_bb});
  ignore (build_br loopcond_bb builder);

  position_at_end loopcond_bb builder;
  let end_cond = match cond with
    | Some(e) -> codegen_expr e
    | None -> const_int bool_type 1 in
  let zero = const_int bool_type 0 in
  let cond_val = build_icmp Icmp.Ne end_cond zero "forendcond" builder in
  ignore (build_cond_br end_cond loopbody_bb endfor_bb builder);

  position_at_end loopbody_bb builder;
  ignore (codegen_stmt s);
  ignore (build_br loopstep_bb builder);

  position_at_end loopstep_bb builder;
  ignore (match step with
    | Some(e) -> ignore(codegen_expr e)
    | None -> ()); 
  ignore (build_br loopcond_bb builder);
  
  ignore (position_at_end endfor_bb builder);

  ignore (match label with 
    | Some(Id(l)) -> Hashtbl.remove labels l
    | None -> Hashtbl.remove labels "currentFor")

| Continue(label) ->
  let jumpLoop = match label with
    | Some(Id(l)) -> Hashtbl.find labels l 
    | None -> Hashtbl.find labels "currentFor" in
  ignore (build_br jumpLoop.cont builder)
| Break(label) ->
  let jumpLoop = match label with
    | Some(Id(l)) -> Hashtbl.find labels l 
    | None -> Hashtbl.find labels "currentFor" in
  ignore (build_br jumpLoop.break builder)
| Return(e) -> 
  exist_ret_stmt := true;
  ignore (match e with
    | Some(ret) -> 
        let vl = codegen_expr ret in
        build_ret vl builder 
    | None -> build_ret_void builder)

    
let rec codegen_decl (decl : declaration) = 
  match decl with
  | Var_declaration(ft, dl) -> 
    (* let currentBlock = insertion_block builder in
    
    position_at_end currentBlock builder;
    let f = block_parent currentBlock in *)
    (* let builder = builder_at (instr_begin (entry_block the_function)) *)
    let llvmtype = ft_to_llvmtype ft in
    
    let mapf (Declarator(Id(var_name), ce)) = 
      let alloca = 
      match ce with 
      (* IF SCOPE IS 0 THEN WE HAVE GLOBALS?*)
      | None -> if !scope = 0 then (declare_global llvmtype var_name the_module) else (build_alloca llvmtype var_name builder)
      | Some(Const_expr(e)) -> 
        let n = codegen_expr e in
        if !scope = 0 then (declare_global llvmtype var_name the_module) else
        (build_array_alloca llvmtype n var_name builder)
      in
      Address_records.variable_push var_name alloca
    in
    ignore(List.map mapf dl); (*in non_type*)
    
  | Fun_declaration(ft,Id(name),pl) ->
    let pltype = List.map (fun (Param(c, f, _)) -> 
                            match c with 
                            | Byvalue -> ft_to_llvmtype f
                            | Byref -> pointer_type (ft_to_llvmtype f)) pl in
    let args_type = Array.of_list pltype in
    let funtype = function_type (ft_to_llvmtype ft) args_type in
    let f =
      match lookup_function name the_module with
      | None -> declare_function name funtype the_module
      (* If 'f' conflicted, there was already something named 'name'. If it
       * has a body, don't allow redefinition or reextern. *)
      | Some f ->
          (* If 'f' already has a body, reject this. *)
          if block_begin f <> At_end f then
            raise (Error "redefinition of function");

          (* If 'f' took a different number of arguments, reject. *)
          if element_type (type_of f) <> funtype then
            raise (Error "redefinition of function with different # args");
          f
    in () (*f*)
    (* let plname = List.map (fun Param(_, _, id) -> id) in
    let args_type = Array.of_list pltype in
        (* Set names for all arguments. *)
        Array.iteri (fun i a ->
          let args = Array.of_list pl in
          let n = args.(i) in
          set_value_name n a;
          ignore(Address_records.variable_push(n, a));
        ) (params f); *)
      

  | Fun_definition(ft,id,pl,dl,sl) -> (
    exist_ret_stmt := false;
    let (Id(name)) = id in
    ignore(Address_records.function_add ());
    (* let the_function = codegen_decl (Fun_declaration(ft, id, pl)) in *)
      (* If this is an operator, install it. 
      begin match proto with
      | Ast.BinOpPrototype (name, args, prec) ->
          let op = name.[String.length name - 1] in
          Hashtbl.add Parser.binop_precedence op prec;
      | _ -> ()
      end; *)


      (*------------*)
      (*---TO SEE---*)
      (*------------*)
      let the_function = (
        let pltype = List.map (fun (Param(c, f, _)) -> 
          match c with 
          | Byvalue -> ft_to_llvmtype f
          | Byref -> pointer_type (ft_to_llvmtype f)) pl in
        let args_type = Array.of_list pltype in
        let funtype = function_type (ft_to_llvmtype ft) args_type in
        let f =
        match lookup_function name the_module with
        | None -> declare_function name funtype the_module
        (* If 'f' conflicted, there was already something named 'name'. If it
        * has a body, don't allow redefinition or reextern. *)
        | Some f ->
        (* If 'f' already has a body, reject this. *)
        if block_begin f <> At_end f then
        raise (Error "redefinition of function");

        (* If 'f' took a different number of arguments, reject. *)
        if element_type (type_of f) <> funtype then
        raise (Error "redefinition of function with different # args");
        f
        in f
      ) in

      (* Create a new basic block to start insertion into. *)
      let bb = append_block context "entry" the_function in
      position_at_end bb builder;

      try
        (* Add all arguments to the symbol table and create their allocas. *)
        create_argument_allocas the_function pl;

        let ret_val = List.map codegen_decl dl in
        let ret_val = List.map codegen_stmt sl in

        (* Finish off the function. *)
      (* print_endline(to_string !exist_ret_stmt); *)
        if not !exist_ret_stmt then
          let _ = build_ret_void builder in

        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function the_function;
        ignore(Address_records.function_delete())
        (* ignore(the_function) *)

      with e ->
        ignore (delete_function the_function);
        ignore(Address_records.function_delete());
        raise e
  )

