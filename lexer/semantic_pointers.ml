open Ast
open Symbol

type semantic_node = program | declaration | declarator | stmt
type result_value_type = LVal | RVal | NULL

let semantic_error_found = ref false
let semantic_errors = ref []
exception Semantic_Error of string
exception End_of_semantic

let check expression t1 t2 =
  match expression with
  | NULL -> (?)
  | None -> ()
  | Some(e) -> check e t1 t2
  | Id(s) -> 
      try 
        let Symbol(id, ft, pl, sc) = (symbol_find s true) in
          if ft != t1 (
            semantic_error_found := true;
            semantic_errors := ("Identifier " ^ id ^ " has type " ^ (fulltype_to_string ft) ^
                                "but type " ^ (fulltype_to_string t1) ^ " was expected", !line_number) :: !semantic_errors
          )
          (* here *)
      with Match_failure _ ->  
        semantic_error_found := true;
        semantic_errors := ("Identifier " ^ id ^ " does not exist", !line_number) :: !semantic_errors
  | True | False ->
  | INT(i) -> check h; check Stmt_block(t)
  | CHAR(c) -> scope_check e; if ((typecheck e) = Type(Bool, 0)) then (check s1) (* else raise exc *)
  | FLOAT(f) ->  
  | Fun_call(Id(s), elist) -> 
  | Table_call(e1, e2) ->    
  | Un_operation(op, e) ->
  | Bin_operation(e1, op, e2) ->
  | Un_assignment_left(op, e) ->
  | Un_assignment_right(e, op) ->
  | Bin_assignment(e1, op, e2) ->
  | Typecast(ft ,e) ->
  | Question(e1, e2, e3) ->
  | New(ft, e) ->
  | Delete(e) ->  

let semantic ast =
  match ast with
  | Var_declaration(_, []) -> ()
  | Var_declaration(ft, h::t) -> 
    let Declarator(Id(s), ce) = h in
      check ce Type(Int, 0) RVal;
      symbol_push Symbol(s, ft, None, !scope);
      semantic Var_declaration(ft, t)
  | Fun_declaration(ft, Id(s), pl) ->
      symbol_push Symbol(s, ft, Some(pl), !scope)
  | Fun_definition(ft, Id(s), pl, dl, sl) ->
      symbol_push Symbol(s, ft, Some(pl), !scope);
      scope_add ();
      List.map push_param pl;
      ret_stack := ft :: !ret_stack;
      List.map semantic dl;
      List.map semantic sl;
      ret_stack := List.tl !ret_stack;
      scope_delete ()
  | Empty_stmt -> ()
  | Expression(e) -> check e NULL NULL
  | Stmt_block(sl) -> 
      scope_add ();
      List.map semantic sl;
      scope_delete ()
  | If(e, s, None) -> 
      check e Type(Bool, 0) RVal;
      semantic s
  | If(e, s1, Some(s2)) -> 
      check e Type(Bool, 0) RVal;
      semantic s1;
      semantic s2
  | For(None, e1, e2, e3, s) ->
      check e1 NULL NULL;
      check e2 Type(Bool, 0) RVal;
      check e3 NULL NULL;
      semantic s
  | For(Some(Id(id)), e1, e2, e3, s) -> 
      check e1 NULL NULL;
      check e2 Type(Bool, 0) RVal;
      check e3 NULL NULL;
      if (symbol_find id false) = NULL then symbol_push Symbol(id, Type(Label), None, !scope)
      else (semantic_error_found := true;
            semantic_errors := ("Label " ^ id ^ " already exists", !line_number) :: !semantic_errors;)
      semantic s
  | Continue(None)-> ()
  | Continue(Some(Id(s))) ->
    if (symbol_find s true) = NULL then (
      semantic_error_found := true;
      semantic_errors := ("Label " ^ s ^ " does not exist", !line_number) :: !semantic_errors)
  | Break(None) -> ()
  | Break(Some(Id(s))) -> 
      if (symbol_find s true) = NULL then (
        semantic_error_found := true;
        semantic_errors := ("Label " ^ s ^ " does not exist", !line_number) :: !semantic_errors)
  | Return(e) -> 
      check e (List.hd !ret_stack) RVal
  | _ -> raise (Semantic_Error ("Semantic was called for sth weird"))

let print_semantic_error () =
  if !semantic_error_found then               
    let f (s,l) =
        "\nSemantic Error:" ^ s ^ " at line: " ^ (string_of_int l)
    in
        raise (Semantic_Error (List.fold_left (^) " " (List.map f (List.rev !semantic_errors))))
  else
      raise End_of_semantic

