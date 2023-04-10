open Ast
open Symbol
open Lexer

let ret_stack = ref []

let semantic_errors = ref []
exception Semantic_Error of string
exception End_of_semantic

let typecheck name ft t1 =
  if ft != t1 
    then semantic_errors := (name ^ " has type " ^ (fulltype_to_string ft) ^
                        "but type " ^ (fulltype_to_string t1) ^ " was expected", !Lexer.line_number) :: !semantic_errors
  else ()

let retcheck name = 
  semantic_errors := (name ^ " has type RVal but type LVal was expected", !Lexer.line_number) :: !semantic_errors

let rec check (expression:expr) t1 t2 =
  match expression with
  | NULL -> () (* ? *)
  | Id(s) -> 
      if (symbol_find s true) = NULL 
        then semantic_errors := ("Identifier " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors
      else (
        let Symbol(id, ft, pl, sc) = (symbol_find s true) 
        in typecheck ("Identifier " ^ id) ft t1)
  | True -> 
      typecheck ("Expression True") (Type(Bool, 0)) t1;
      if t2 = LVal then retcheck "Expression True"
  | False -> 
      typecheck ("Expression False") (Type(Bool, 0)) t1;
      if t2 = LVal then retcheck "Expression False"
  | INT(i) -> 
      typecheck ("Expression " ^ (string_of_int i)) (Type(Int, 0)) t1;
      if t2 = LVal then retcheck ("Expression " ^ (string_of_int i))
  | CHAR(c) -> 
      typecheck ("Expression " ^ (String.make 1 c)) (Type(Char, 0)) t1;
      if t2 = LVal then retcheck ("Expression " ^ (String.make 1 c))
  | FLOAT(f) -> 
      typecheck ("Expression " ^ (string_of_float f)) (Type(Double, 0)) t1;
      if t2 = LVal then retcheck ("Expression " ^ (string_of_float f))
  | STRING(s) ->
      typecheck ("String " ^ s) (Type(Double, 0)) t1;
      if t2 = LVal then retcheck ("String " ^ s)
  | Fun_call(Id(s), elist) -> 
      let ret = (symbol_find s true) in
      if ret = NULL 
        then semantic_errors := ("Function " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors
      else
        let Symbol(id, ft, pl, sc) = ret 
        in 
          typecheck ("Function " ^ id) ft t1;
          if t2 = LVal then retcheck ("Function call " ^ id);
          if pl = None then ()
          else(
            let Some(plist) = pl in
              if List.length elist != List.length plist 
                then semantic_errors := ("Function call " ^ s ^ " expects " ^ string_of_int (List.length plist) ^ " arguments but " ^ string_of_int (List.length elist) ^ " were given", !Lexer.line_number) :: !semantic_errors
              else
                let foo p e =
                  match p with 
                  | Param(Byref, ft, id) -> check e ft LVal
                  | Param(Byvalue, ft, id) -> check e ft RVal
                in 
                let _ = List.map2 foo plist elist in ())        
  | Table_call(e1, e2) -> ()
  | Un_operation(op, e) -> ()
  | Bin_operation(e1, op, e2) -> ()
  | Un_assignment_left(op, e) -> ()
  | Un_assignment_right(e, op) -> ()
  | Bin_assignment(e1, op, e2) -> ()
  | Typecast(ft, e) -> ()
  | Question(e1, e2, e3) -> ()
  | New(ft, e) -> ()
  | Delete(e) ->  ()

let rec semantic node =
  match node with
  | Var_declaration(_, []) -> ()
  | Var_declaration(ft, Declarator(Id(s), ce)::t) -> 
      if ce = None then (
        symbol_push (Symbol(s, ft, None, !scope));
        semantic (Var_declaration(ft, t)))
      else (
        let Some(Const_expr(e)) = ce in 
        check e (Type(Int, 0)) RVal;
        symbol_push (Symbol(s, ft, None, !scope));
        semantic (Var_declaration(ft, t)))
  | Fun_declaration(ft, Id(s), pl) ->
      symbol_push (Symbol(s, ft, Some(pl), !scope))
  | Fun_definition(ft, Id(s), pl, dl, sl) ->
      symbol_push (Symbol(s, ft, Some(pl), !scope));
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
      check e (Type(Bool, 0)) RVal;
      semantic s
  | If(e, s1, Some(s2)) -> 
      check e (Type(Bool, 0)) RVal;
      semantic s1;
      semantic s2
  | For(None, e1, e2, e3, s) ->
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> check e t1 t2
      in 
      List.map foo ([(e3, NULL, NULL); (e2, (Type(Bool, 0)), RVal); (e3, NULL, NULL)]);
      semantic s
  | For(Some(Id(id)), e1, e2, e3, s) -> 
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> check e t1 t2
      in 
      List.map foo ([(e3, NULL, NULL); (e2, (Type(Bool, 0)), RVal); (e3, NULL, NULL)]);
      if (symbol_find id false) = NULL then symbol_push (Symbol(id, (Type(Label, 0)), None, !scope))
      else semantic_errors := ("Label " ^ id ^ " already exists", !Lexer.line_number) :: !semantic_errors;
      semantic s
  | Continue(None)-> ()
  | Continue(Some(Id(s))) ->
    if (symbol_find s true) = NULL 
      then semantic_errors := ("Label " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors
  | Break(None) -> ()
  | Break(Some(Id(s))) -> 
      if (symbol_find s true) = NULL 
        then semantic_errors := ("Label " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors
  | Return(None) -> ()
  | Return(Some(e)) ->
      check e (List.hd !ret_stack) RVal
  | _ -> raise (Semantic_Error ("Semantic was called for sth weird"))

let print_semantic_error () =
  match !semantic_errors with
  | [] -> raise End_of_semantic
  | _ ->  
    let f (s,l) =
    "\nSemantic Error:" ^ s ^ " at line: " ^ (string_of_int l)
    in
    raise (Semantic_Error (List.fold_left (^) " " (List.map f (List.rev !semantic_errors))))

