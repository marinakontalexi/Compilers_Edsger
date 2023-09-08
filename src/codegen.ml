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
let named_funcs = Hashtbl.create 10

(* types *)
let int_type = i16_type context
let char_type = i8_type context
let double_type = x86fp80_type context
let bool_type = i1_type context
let non_type = void_type context

type result_value_type = LVAL | RVAL | NULL

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
  let f_help (Param(call,ft,Id(s))) =  (ft_to_llvmtype ft, s, call) in
  let args = Array.of_list (List.map f_help pl) in
  Array.iteri (fun i ai ->
    let (ft, var_name, call) = args.(i) in
    match call with
    | Byvalue ->
      let alloca = create_entry_block_alloca the_function ft var_name in
      (* Store the initial value into the alloca. *)
        ignore(build_store ai alloca builder);
      (* Add arguments to variable symbol table. *)
      (* Hashtbl.add named_values var_name alloca; *)
      ignore(Address_records.variable_push var_name alloca)
    | _ -> ignore(Address_records.variable_push var_name ai)
  ) (params the_function)

  let rec codegen_expr (value_type:result_value_type) (expression:expr)  = 
    let name = expr_to_string expression in
    match expression with
    | NULL-> const_null (pointer_type int_type)
  
    | Id(s) -> 
      let Var(_, alloca, _) = variable_find s in
      (match value_type with
      | LVAL -> alloca
      | RVAL | NULL -> build_load alloca s builder )
  
    | True -> const_int bool_type 1
    | False -> const_int bool_type 0
    | INT(i) -> const_int int_type i
    | CHAR(c) -> let ascii = Char.code c in const_int char_type ascii 
    | FLOAT(f) -> const_float double_type f
    | STRING(s) -> 
      let str_init = const_stringz context s in 
      let str = define_global ".str" str_init the_module in
      let constzero = const_int int_type 0 in 
      build_gep str [|constzero;constzero|] "strtmp" builder 
      (* str *)
  
    (* -------------- *)
    (* --- TO SEE --- *)
    (* -------------- *)
    | Fun_call(Id(callee_name), args) -> 
      (match lookup_function callee_name the_module with
        | Some callee -> 
          let call_list = 
            try Hashtbl.find named_funcs (Hashtbl.hash callee_name)
          with Not_found -> raise (Error "stupid dictionary") 
        in 
          let val_list = List.map (fun c -> match c with Byvalue -> RVAL | Byref -> LVAL) call_list in 
          let args = Array.of_list (List.map2 codegen_expr val_list args) in
          build_call callee args "" builder 
        | None -> raise (Error "unknown function referenced"))
      (*THIS SHOULD CHANGE |BYREF->LVAL |BYVALUE->RVAL *)
      
  
    | Table_call(e1, e2) -> 
      let ptr = codegen_expr RVAL e1 in 
      let offset = codegen_expr RVAL e2 in
      let p = build_gep ptr [|offset|] "tableCalltmp" builder in
      (match value_type with
      | LVAL -> p
      | RVAL -> build_load p "loadtmp" builder)
  
    | Un_operation(op, e) ->
      let vl = codegen_expr RVAL e in
      (match op with
      | AND -> codegen_expr LVAL e
      | POINT -> 
        (match value_type with
        | RVAL -> build_load vl "loadtmp" builder
        | LVAL -> vl)
      | EXC -> build_not vl "nottmp" builder
      | POS -> vl
      | NEG -> 
        if ((type_of vl) = int_type) then build_neg vl "negtmp" builder
        else build_fneg vl "negtmp" builder)
  
    | Bin_operation(e1, op, e2) ->(
      let e1_val = codegen_expr RVAL e1 in
      let e2_val = codegen_expr RVAL e2 in
      let val_type = (match classify_type (type_of e1_val) with 
        |Pointer -> element_type (type_of e1_val)
        | _ -> type_of e1_val) in
      let vt1 = type_of e1_val in
      let vt2 = type_of e2_val in 
      let isInt = (vt1 = int_type && vt2 = int_type) in
      let isDouble = (vt1 = double_type && vt2 = double_type) in
      match op with
      | PLUS ->
        (* if (size_of val_type) = (size_of (pointer_type int_type)) then
          build_gep e1_val [|e2_val|] "ptraddtmp" builder *)
        if isInt  then build_add e1_val e2_val "addtmp" builder
        else if isDouble then build_fadd e1_val e2_val "addtmp" builder
        else build_gep e1_val [|e2_val|] "ptraddtmp" builder
      | MINUS ->
        (* if (size_of val_type) = (size_of (pointer_type int_type)) then
          build_gep e1_val [|e2_val|] "ptraddtmp" builder *)
        if isInt then build_sub e1_val e2_val "subtmp" builder
        else if isDouble then build_fsub e1_val e2_val "subtmp" builder
        else build_gep e1_val [|e2_val|] "ptraddtmp" builder
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
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Slt e1_val e2_val "lesstmp" builder
        else build_fcmp Fcmp.Olt e1_val e2_val "lesstmp" builder
      | MORE ->
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sgt e1_val e2_val "moretmp" builder
        else build_fcmp Fcmp.Ogt e1_val e2_val "moretmp" builder
      | LEQ ->
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sle e1_val e2_val "leqtmp" builder
        else build_fcmp Fcmp.Oge e1_val e2_val "leqtmp" builder
      | GEQ ->
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Sge e1_val e2_val "geqtmp" builder
        else build_fcmp Fcmp.Oge e1_val e2_val "geqtmp" builder
      | EQ ->
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Eq e1_val e2_val "eqtmp" builder
        else build_fcmp Fcmp.Oeq e1_val e2_val "eqtmp" builder
      | NEQ ->
        if (val_type = int_type || val_type = bool_type || (size_of val_type) = (size_of (pointer_type int_type))) then
          build_icmp Icmp.Ne e1_val e2_val "neqtmp" builder
        else build_fcmp Fcmp.One e1_val e2_val "neqtmp" builder

      | LOGICAL_AND -> 
        let and_val = build_and e1_val e2_val "andtmp" builder in
        let one = const_int bool_type 1 in 
        build_icmp Icmp.Eq and_val one "andeqtmp" builder
      | LOGICAL_OR -> 
        let or_val = build_or e1_val e2_val "andtmp" builder in
        let one = const_int bool_type 1 in 
        build_icmp Icmp.Eq or_val one "andeqtmp" builder
      | COMMA -> e2_val
      )

    | Un_assignment_left(INCR, e) ->
      let lval = codegen_expr LVAL e in
      let vl = codegen_expr RVAL (Bin_operation(e, PLUS, INT(1))) in
      let _ = build_store vl lval builder in 
      vl
    | Un_assignment_right(e, INCR)-> 
      let rval = codegen_expr RVAL e in
      let lval = codegen_expr LVAL e in
      let vl = codegen_expr RVAL (Bin_operation(e, PLUS, INT(1))) in
      let _ = build_store vl lval builder in
      rval
    | Un_assignment_left(DECR, e) ->
      let lval = codegen_expr LVAL e in
      let vl = codegen_expr RVAL (Bin_operation(e, MINUS, INT(1))) in
      let _ = build_store vl lval builder in
      vl
    | Un_assignment_right(e, DECR) -> 
      let rval = codegen_expr RVAL e in
      let lval = codegen_expr LVAL e in
      let vl = codegen_expr RVAL (Bin_operation(e, MINUS, INT(1))) in
      let _ = build_store vl lval builder in
      rval
  
    | Bin_assignment (e1, ass, e2) ->
      let e1_val = codegen_expr LVAL e1 in
      let vl = match ass with
        | TIMESEQ -> codegen_expr RVAL (Bin_operation(e1, TIMES, e2))
        | MODEQ  -> codegen_expr RVAL (Bin_operation(e1, MOD, e2))
        | DIVEQ -> codegen_expr RVAL (Bin_operation(e1, DIV, e2))
        | PLUSEQ -> codegen_expr RVAL (Bin_operation(e1, PLUS, e2))
        | MINUSEQ -> codegen_expr RVAL (Bin_operation(e1, MINUS, e2))
        | ASSIGN -> codegen_expr RVAL e2
      in 
      let _ = build_store vl e1_val builder in
      vl
  
    | Typecast(fullt, e) -> (
      let vl = codegen_expr RVAL e in
      let fromType = type_of vl in
      (* print_endline (string_of_lltype fromType);
      print_endline (string_of_bool (fromType = double_type)); *)
      let ft = ft_to_llvmtype fullt in
      (* print_endline (string_of_lltype ft);
      print_endline (string_of_bool (ft = double_type)); *)
      if(ft = int_type) then (
        if (ft = int_type) then (
          if (fromType = int_type) then build_zext vl ft "casttmp" builder
          else if fromType = double_type then ( build_fptosi vl ft "casttmp" builder)
          else (build_ptrtoint vl ft "casttmp" builder))

        else if (ft = char_type) then (
          if (fromType = int_type) then (build_trunc vl ft "casttmp" builder)
          else if (fromType = double_type) then (build_fptoui vl ft "casttmp" builder)
          else (build_ptrtoint vl ft "casttmp" builder))
        
        else( 
          if (fromType = int_type) then (build_icmp Icmp.Ne vl (const_int fromType 0) "casttmp" builder)
          else if (fromType = double_type) then (build_fcmp Fcmp.One vl (const_float double_type 0.0) "casttmp" builder)
          else (build_ptrtoint vl ft "casttmp" builder)
          )
        )
    
      else if (ft = double_type) then (
        if (fromType = int_type) then 
          (build_sitofp vl double_type "casttmp" builder)
        else 
          (build_uitofp vl double_type "casttmp" builder)
      )
    
      else (build_bitcast vl ft "casttmp" builder)
      
    )
    
    | Question(e1, e2, e3)->
      let cond = codegen_expr RVAL e1 in
      let zero = const_int bool_type 0 in
      let cond_val = build_icmp Icmp.Ne cond zero "question" builder in
      let start_bb = insertion_block builder in
      let f = block_parent start_bb in
      let trueBB = append_block context "questionTrue" f in
      let falseBB = append_block context "questionFalse" f in
      let afterbb = append_block context "endQuestion" f in
      ignore(build_cond_br cond trueBB falseBB builder);
      
      position_at_end trueBB builder;
      let vl2 = codegen_expr RVAL e2 in
      ignore(build_br afterbb builder);

      position_at_end falseBB builder;
      let vl3 = codegen_expr RVAL e3 in
      ignore(build_br afterbb builder);

      position_at_end afterbb builder;
      if (type_of vl2 <> void_type context) then 
        build_phi [(vl2, trueBB); (vl3, falseBB)] "questiontmp" builder
      else undef (void_type context)

    | New(ft, None)->
      let typ = ft_to_llvmtype ft in
      build_array_malloc typ (const_int int_type 0) "newtmp" builder
    | New(ft, Some(e))->
      let vl = codegen_expr RVAL e in
      let typ = ft_to_llvmtype ft in
      build_array_malloc typ vl "newtmp" builder
    | Delete(e) ->
      let vl = codegen_expr RVAL e in 
      build_free vl builder

let rec codegen_stmt (statement:stmt) = 
match statement with
| Empty_stmt -> ()
| Expression(e) -> ignore (codegen_expr RVAL e)
| Stmt_block(sl) -> 
  (* let start_bb = insertion_block builder in 
  let f = block_parent start_bb in
  let new_bb = append_block context "stmtBlock" f in
  ignore (build_br new_bb) *)
  ignore (List.map codegen_stmt sl)
| If(e,s_then, s_else) ->
    let cond = codegen_expr RVAL e in
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
    
    if (block_terminator @@ insertion_block builder) = None then 
    ignore (build_br end_if builder) else ();
    
    position_at_end else_bb builder;
    ignore(match s_else with
    | Some s -> codegen_stmt s
    | None -> ());

    if (block_terminator @@ insertion_block builder) = None then
    ignore (build_br end_if builder) else ();

    ignore (position_at_end end_if builder)

| For(label, init, cond, step, s) ->
  ignore (match init with
    | Some(e) -> ignore(codegen_expr RVAL e)
    | None -> ());
  let prev_bb = insertion_block builder in
  let f = block_parent prev_bb in
  let loopcond_bb = append_block context "loopcond" f in
  let loopbody_bb = append_block context "loopbody" f in
  let loopstep_bb = append_block context "loopstep" f in
  let endfor_bb = append_block context "endfor" f in
  ignore(Hashtbl.add labels "currentFor" {cont = loopstep_bb; break = endfor_bb});
  ignore (match label with
    (* Add label to labels *)
    | Some(Id(id)) -> ignore(Hashtbl.add labels id {cont = loopstep_bb; break = endfor_bb})
    | None -> ());
  ignore (build_br loopcond_bb builder);

  position_at_end loopcond_bb builder;
  let end_cond = match cond with
    | Some(e) -> codegen_expr RVAL e
    | None -> const_int bool_type 1 in
  let zero = const_int bool_type 0 in
  let cond_val = build_icmp Icmp.Ne end_cond zero "forendcond" builder in
  ignore (build_cond_br end_cond loopbody_bb endfor_bb builder);

  position_at_end loopbody_bb builder;
  ignore (codegen_stmt s);
  if (block_terminator @@ insertion_block builder) = None then (
  ignore (build_br loopstep_bb builder));

  position_at_end loopstep_bb builder;
  ignore (match step with
    | Some(e) -> ignore(codegen_expr RVAL e)
    | None -> ()); 
  ignore (build_br loopcond_bb builder);
  
  ignore (position_at_end endfor_bb builder);
  ignore (Hashtbl.remove labels "currentFor");
  ignore (match label with 
    | Some(Id(l)) -> ignore(Hashtbl.remove labels l)
    | None -> ())

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
  (* exist_ret_stmt := true; *)
  ignore (match e with
    | Some(ret) -> 
        let vl = codegen_expr RVAL ret in
        build_ret vl builder 
    | None -> build_ret_void builder)

let rec codegen_decl (decl : declaration) = 
  let cg decl = match decl with
  | Var_declaration(ft, dl) -> codegen_vardecl ft dl
  | Fun_declaration(ft,Id(name),pl) -> codegen_fdecl ft name pl
  | Fun_definition(ft,id,pl,dl,sl) -> codegen_fdef ft id pl dl sl
  in
  if !scope = 0 then cg decl
  else (
    let parent = insertion_block builder in
    cg decl; 
    ignore @@ position_at_end parent builder
  )

and codegen_vardecl ft dl = 
    let llvmtype = ft_to_llvmtype ft in
    
    let mapf (Declarator(Id(var_name), ce)) = 
      let alloca = 
      match ce with 
      (* IF SCOPE IS 0 THEN WE HAVE GLOBALS?*)
      | None -> if !scope = 0 then (declare_global llvmtype var_name the_module) else (build_alloca llvmtype var_name builder)
      | Some(Const_expr(e)) -> 
        let n = codegen_expr RVAL e in
        if !scope = 0 then (
          let Some(temp) = (n |> Llvm.int64_of_const) in
          let int_n = Int64.to_int temp in
          let init = if llvmtype = int_type then (const_int int_type 0) else (const_float double_type 0.0) in
          let arr = const_array llvmtype (Array.make int_n init) in 
          define_global var_name arr the_module
        ) 
      else (build_array_alloca (pointer_type llvmtype) n var_name builder)
      in
      Address_records.variable_push var_name alloca
    in
    ignore(List.map mapf dl) (*in non_type*)
    
and codegen_fdecl ft name pl = 
  let call_list = List.map (fun (Param(c, _, _)) -> c) pl in
  Hashtbl.add named_funcs (Hashtbl.hash name) call_list;
  let pltype = List.map (fun (Param(c, f, _)) -> (*ft_to_llvmtype f) pl*)
                          match c with 
                          | Byvalue -> ft_to_llvmtype f
                          | Byref -> pointer_type (ft_to_llvmtype f)) pl
                        in
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
  in () 


and codegen_fdef ft id pl dl sl = 
    (* exist_ret_stmt := false; *)
    let (Id(name)) = id in
    let call_list = List.map (fun (Param(c, _, _)) -> c) pl in
    Hashtbl.add named_funcs (Hashtbl.hash name) call_list;
    ignore(Address_records.function_add ());
      let the_function = (
        let pltype = List.map (fun (Param(c, f, _)) -> (*ft_to_llvmtype f) pl*)
          match c with 
          | Byvalue -> ft_to_llvmtype f
          | Byref -> pointer_type (ft_to_llvmtype f)) pl 
        in
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
        (* if not !exist_ret_stmt then( *)
        if (block_terminator @@ insertion_block builder) = None then (
          ignore(build_ret_void builder));

        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function the_function;
        ignore(Address_records.function_delete())
        (* ignore(the_function) *)

      with e ->
        ignore (delete_function the_function);
        ignore(Address_records.function_delete());
        raise e


  





(* let rec codegen_decl (decl : declaration) = 
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
        let n = codegen_expr RVAL e in
        if !scope = 0 then (declare_global llvmtype var_name the_module) else
        (build_array_alloca (pointer_type llvmtype) n var_name builder)
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
    (* exist_ret_stmt := false; *)
    let (Id(name)) = id in
    ignore(Address_records.function_add ());
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
        (* if not !exist_ret_stmt then( *)
        if (block_terminator @@ insertion_block builder) = None then (
          ignore(build_ret_void builder));

        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function the_function;
        ignore(Address_records.function_delete())
        (* ignore(the_function) *)

      with e ->
        ignore (delete_function the_function);
        ignore(Address_records.function_delete());
        raise e
  ) *)

