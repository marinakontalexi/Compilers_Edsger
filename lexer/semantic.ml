open Ast
open Symbol
open Lexer

let ret_stack = ref []

let semantic_errors = ref []
exception Semantic_Error of string
exception End_of_semantic

let typecheck name (ft:fulltype) (t1:fulltype) =
  if t1 = NULL || ft = t1 then ()
  else semantic_errors := (name ^ " has type " ^ (fulltype_to_string ft) ^
                        " but type " ^ (fulltype_to_string t1) ^ " was expected", !Lexer.line_number) :: !semantic_errors;
  ft

let retcheck name = 
  semantic_errors := (name ^ " has type RVal but type LVal was expected", !Lexer.line_number) :: !semantic_errors

let rec check (expression:expr) t1 t2 =
  match expression with
  | NULL -> (NULL:fulltype)
  | Id(s) -> 
    if (symbol_find s true) = NULL 
      then (semantic_errors := ("Identifier " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors; NULL)
    else (
      let Symbol(id, ft, pl, sc) = (symbol_find s true) 
      in typecheck ("Identifier " ^ id) ft t1)
  | True -> 
    if t2 = LVal then retcheck "Expression True";
    typecheck ("Expression True") (Type(Bool, 0)) t1;
  | False -> 
    if t2 = LVal then retcheck "Expression False";
    typecheck ("Expression False") (Type(Bool, 0)) t1
  | INT(i) -> 
    if t2 = LVal then retcheck ("Expression " ^ (string_of_int i));
    typecheck ("Expression " ^ (string_of_int i)) (Type(Int, 0)) t1
  | CHAR(c) -> 
    if t2 = LVal then retcheck ("Expression " ^ (String.make 1 c));
    typecheck ("Expression " ^ (String.make 1 c)) (Type(Char, 0)) t1
  | FLOAT(f) -> 
    if t2 = LVal then retcheck ("Expression " ^ (string_of_float f));
    typecheck ("Expression " ^ (string_of_float f)) (Type(Double, 0)) t1
  | STRING(s) ->
    if t2 = LVal then retcheck ("String " ^ s);
    typecheck ("String " ^ s) (Type(Char, 1)) t1

  | Fun_call(Id(s), elist) -> 
    let ret = (symbol_find s true) in
    if ret = NULL 
      then (semantic_errors := ("Function " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors; NULL)
    else
      let Symbol(id, ft, pl, sc) = ret 
      in 
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
              let _ = List.map2 foo plist elist in ());
        typecheck ("Function " ^ id) ft t1      
  | Table_call(e1, e2) -> (NULL)

  | Un_operation(op, e) -> (NULL)

  | Bin_operation(e1, LOGICAL_AND, e2) 
  | Bin_operation(e1, LOGICAL_OR, e2) -> 
    let _ = check e1 (Type(Bool, 0)) RVal
    and _ = check e2 (Type(Bool, 0)) RVal
    in Type(Bool, 0) 
  | Bin_operation(e1, op, e2) -> (NULL)

  | Un_assignment_left(INCR, e) 
  | Un_assignment_right(e, INCR) -> 
    let e = Bin_assignment(e, ASSIGN, Bin_operation(e, PLUS, INT(1))) in
    check e t1 t2
  | Un_assignment_left(DECR, e) 
  | Un_assignment_right(e, DECR) -> 
    let e = Bin_assignment(e, ASSIGN, Bin_operation(e, MINUS, INT(1))) in
    check e t1 t2

  | Bin_assignment(e1, TIMESEQ, e2) -> 
    let e = Bin_assignment(e1, ASSIGN, Bin_operation(e1, TIMES, e2)) in
    check e t1 t2
  | Bin_assignment(e1, MODEQ, e2) -> 
    let e = Bin_assignment(e1, ASSIGN, Bin_operation(e1, MOD, e2)) in
    check e t1 t2
  | Bin_assignment(e1, DIVEQ, e2) -> 
    let e = Bin_assignment(e1, ASSIGN, Bin_operation(e1, DIV, e2)) in
    check e t1 t2
  | Bin_assignment(e1, PLUSEQ, e2) -> 
    let e = Bin_assignment(e1, ASSIGN, Bin_operation(e1, PLUS, e2)) in
    check e t1 t2
  | Bin_assignment(e1, MINUSEQ, e2) -> 
    let e = Bin_assignment(e1, ASSIGN, Bin_operation(e1, MINUS, e2)) in
    check e t1 t2
  | Bin_assignment(e1, ASSIGN, e2) ->
    let type1 = check e1 NULL LVal in
    check e2 type1 RVal  

  | Typecast(ft, e) -> (NULL)
  | Question(e1, e2, e3) -> 
    let _ = check e1 (Type(Bool, 0)) RVal
    and type2 = check e2 NULL RVal in
      check e3 type2 RVal
  | New(ft, None) -> 
      check (New(ft, Some(INT(1)))) t1 t2
  | New(ft, Some(e)) -> 
    let _ = check e (Type(Int, 0)) RVal in
      (match ft with
      | Type(Void, _) 
      | Type(Label, _)
      | Type(Any, _) -> typecheck "New t" ft (Type(Any, 0))
      | Type(t, n) -> typecheck "New t" (Type(t, (n + 1))) t1)      
  | Delete(e) ->  
    let t = check e NULL NULL in
    match t with
    | Type(Void, _) 
    | Type(Label, _)
    | Type(Any, _) -> typecheck "Delete e" t (Type(Any, 1))
    | Type(t, n) -> 
      if n = 0 then typecheck "Delete e" (Type(t, n)) (Type(t, (n+1)))
      else Type(t, n)

let rec semantic node =
  match node with
  | Var_declaration(_, []) -> ()
  | Var_declaration(ft, Declarator(Id(s), ce)::t) -> 
      if ce = None then (
        symbol_push (Symbol(s, ft, None, !scope));
        semantic (Var_declaration(ft, t)))
      else (
        let Some(Const_expr(e)) = ce in 
        let _ = check e (Type(Int, 0)) RVal in
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
  | Expression(e) -> 
    let _ = check e NULL NULL in ()
  | Stmt_block(sl) -> 
    scope_add ();
    List.map semantic sl;
    scope_delete ()
  | If(e, s, None) -> 
    let _ = check e (Type(Bool, 0)) RVal in
      semantic s
  | If(e, s1, Some(s2)) -> 
    let _ = check e (Type(Bool, 0)) RVal in
      semantic s1;
      semantic s2
  | For(None, e1, e2, e3, s) ->
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> let _ = check e t1 t2 in ()
      in 
      List.map foo ([(e3, NULL, NULL); (e2, (Type(Bool, 0)), RVal); (e3, NULL, NULL)]);
      semantic s
  | For(Some(Id(id)), e1, e2, e3, s) -> 
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> let _ = check e t1 t2 in ()
      in 
      List.map foo ([(e3, NULL, NULL); (e2, (Type(Bool, 0)), RVal); (e3, NULL, NULL)]);
      if (symbol_find id false) = NULL then symbol_push (Symbol(id, (Type(Label, 0)), None, !scope))
      else semantic_errors := ("Label " ^ id ^ " already exists", !Lexer.line_number) :: !semantic_errors;
      semantic s
  | Continue(None)
  | Break(None) -> ()
  | Continue(Some(Id(s)))
  | Break(Some(Id(s))) -> 
    if (symbol_find s true) = NULL 
      then semantic_errors := ("Label " ^ s ^ " does not exist", !Lexer.line_number) :: !semantic_errors
    else let _ = check (Id(s)) (Type(Label, 0)) RVal in ()
  | Return(None) -> ()
  | Return(Some(e)) ->
      let _ = check e (List.hd !ret_stack) RVal in ()
  | _ -> raise (Semantic_Error ("Semantic was called for sth weird"))

let print_semantic_error () =
  match !semantic_errors with
  | [] -> raise End_of_semantic
  | _ ->  
    let f (s,l) =
    "Semantic Error: " ^ s ^ " at line: " ^ (string_of_int l) ^ "\n"
    in
    raise (Semantic_Error (List.fold_left (^) "" (List.map f (List.rev !semantic_errors))))

