open Ast
open Symbol
open Lexer

let ret_stack = ref []

let semantic_errors = ref []
exception Semantic_Error of string
exception End_of_semantic

let retcheck name r = 
  if r = LVal then ()
  else semantic_errors := (name ^ " has type RVal but type LVal was expected", !Lexer.line_number) :: !semantic_errors

let rec typecheck expression (t1:fulltype) (t2:result_value_type) t =
  let (ft, resulttype) = 
    if t = None then gettype expression
    else let Some(tp) = t in tp
  and name = expr_to_string expression in
    if t1 = NULL || ft_equal ft t1 || ft = NULL then ()
    else (semantic_errors := (name ^ " has type " ^ (fulltype_to_string ft) ^
                          " but type " ^ (fulltype_to_string t1) ^ " was expected", !Lexer.line_number) :: !semantic_errors);
    if t2 = LVal then retcheck name resulttype

and gettype (expression:expr) =
  let name = expr_to_string expression in
  match expression with
  | NULL -> ((NULL:fulltype), (NULL:result_value_type))
  | Id(s) -> 
    (try 
      let Symbol(id, ft, pl, sc) = symbol_find s true in
      (ft, LVal)
    with 
      Match_failure _ ->
      semantic_errors := (name ^ " does not exist", !Lexer.line_number) :: !semantic_errors; 
      (NULL, NULL))
  | True | False -> ((Type(Bool, 0)), RVal)
  | INT(_) -> ((Type(Int, 0)), RVal)
  | CHAR(_) -> ((Type(Char, 0)), RVal)
  | FLOAT(_) -> ((Type(Double, 0)), RVal)
  | STRING(_) -> ((Type(Char, 1)), RVal)
  | Fun_call(Id(s), elist) -> 
    (try 
      let Symbol(id, ft, pl, sc) = symbol_find s true in
        if pl = None then (NULL,NULL)
        else 
          let Some(plist) = pl in
            (if List.length elist != List.length plist 
              then semantic_errors := (name ^ " expects " ^ string_of_int (List.length plist) ^ " arguments but " ^ string_of_int (List.length elist) ^ " were given", !Lexer.line_number) :: !semantic_errors
            else
              let foo p e =
                match p with 
                | Param(Byref, pft, _) -> typecheck e pft LVal None
                | Param(Byvalue, pft, _) -> typecheck e pft RVal None
              in 
              let _ = List.map2 foo plist elist in ());
              (ft, RVal)   
    with
      Match_failure _ ->
      semantic_errors := (name ^ " does not exist", !Lexer.line_number) :: !semantic_errors; 
      (NULL, NULL))
  | Table_call(e1, e2) -> 
    typecheck e2 (Type(Int, 0)) RVal None;
    (try
      let (Type(t,n), v) = gettype e1 in
      typecheck e1 (Type(Basic, 1)) LVal (Some(Type(t,n), v));
      (Type(t, n-1), LVal)
    with
    Match_failure _ -> (NULL, NULL))
  
  | Un_operation(AND, e) -> 
    (try 
      let (Type(t, n), v) = gettype e in
      typecheck e (Type(Basic, -1)) LVal (Some((Type(t, n)), v));
      (Type(t, n+1), RVal)
    with 
      Match_failure _ -> (NULL, NULL))
  | Un_operation(POINT, e) -> 
    (try 
      let (Type(t, n), v) = gettype e in
      typecheck e (Type(Basic, 1)) LVal (Some((Type(t, n)), v));
      (Type(t, n-1), LVal)
    with 
      Match_failure _ -> (NULL, NULL))
  | Un_operation(EXC, e) -> 
    typecheck e (Type(Bool, 0)) RVal None;
    ((Type(Bool, 0)), RVal)
  | Un_operation(POS, e)
  | Un_operation(NEG, e) -> 
    let (t,v) = gettype e in
    typecheck e (Type(Num, 0)) RVal (Some((t,v)));
    (t,v)

  | Bin_operation(e1, PLUS, e2)
  | Bin_operation(e1, MINUS, e2) ->
    let (t,v) = gettype e1 in
      if ft_equal t (Type(Num, 0)) then
        (typecheck e2 t RVal None;
        (t, RVal))
      else if ft_equal t (Type(Basic, 1)) then
        (typecheck e2 (Type(Int, 0)) RVal None;
        (t, RVal))
      else 
        (semantic_errors := (name ^ " has type " ^ (fulltype_to_string t) ^
                          " but type " ^ (fulltype_to_string (Type(Num, 0))) ^ " or " ^ 
                          (fulltype_to_string (Type(Basic, 1))) ^ " was expected", !Lexer.line_number) :: !semantic_errors; (NULL, NULL))
  | Bin_operation(e1, TIMES, e2)
  | Bin_operation(e1, DIV, e2) ->
    let (t,v) = gettype e1 in
      typecheck e1 (Type(Num,0)) RVal (Some((t,v)));
      typecheck e2 t RVal None;
      (t, RVal)
  | Bin_operation(e1, MOD, e2) ->
    typecheck e1 (Type(Int,0)) RVal None;
    typecheck e2 (Type(Int,0)) RVal None;
    (Type(Int,0), RVal)
  | Bin_operation(e1, LESS, e2)
  | Bin_operation(e1, MORE, e2)
  | Bin_operation(e1, LEQ, e2)
  | Bin_operation(e1, GEQ, e2)
  | Bin_operation(e1, EQ, e2)
  | Bin_operation(e1, NEQ, e2) -> 
    let (t,v) = gettype e1 in
      typecheck e1 (Type(Basic,-1)) RVal (Some((t,v)));
      typecheck e2 t RVal None;
      ((Type(Bool, 0)), RVal)
    
  | Bin_operation(e1, LOGICAL_AND, e2) 
  | Bin_operation(e1, LOGICAL_OR, e2) -> 
    typecheck e1 (Type(Bool, 0)) RVal None;
    typecheck e2 (Type(Bool, 0)) RVal None;
    (Type(Bool, 0), RVal)
  | Bin_operation(e1, COMMA, e2) -> 
    typecheck e1 (Type(BV, -1)) RVal None;
    gettype e2

  | Un_assignment_left(INCR, e) 
  | Un_assignment_right(e, INCR) -> 
    gettype (Bin_assignment(e, ASSIGN, Bin_operation(e, PLUS, INT(1))))
  | Un_assignment_left(DECR, e) 
  | Un_assignment_right(e, DECR) -> 
    gettype (Bin_assignment(e, ASSIGN, Bin_operation(e, MINUS, INT(1))))

  | Bin_assignment(e1, TIMESEQ, e2) -> 
    gettype (Bin_assignment(e1, ASSIGN, Bin_operation(e1, TIMES, e2)))
  | Bin_assignment(e1, MODEQ, e2) -> 
    gettype (Bin_assignment(e1, ASSIGN, Bin_operation(e1, MOD, e2)))
  | Bin_assignment(e1, DIVEQ, e2) -> 
    gettype (Bin_assignment(e1, ASSIGN, Bin_operation(e1, DIV, e2)))
  | Bin_assignment(e1, PLUSEQ, e2) -> 
    gettype (Bin_assignment(e1, ASSIGN, Bin_operation(e1, PLUS, e2)))
  | Bin_assignment(e1, MINUSEQ, e2) -> 
    gettype (Bin_assignment(e1, ASSIGN, Bin_operation(e1, MINUS, e2)))
  | Bin_assignment(e1, ASSIGN, e2) ->
    let (t,v) = gettype e1 in
      typecheck e1 (Type(Basic,-1)) LVal (Some((t,v)));
      typecheck e2 t RVal None;
      (t, RVal)

  | Typecast(ft, e) -> 
    typecheck e (Type(Basic, -1)) RVal None;
    (ft, RVal)
  | Question(e1, e2, e3) -> 
    let (t, v) = gettype e2 in
      typecheck e1 (Type(Bool, 0)) RVal None;
      typecheck e3 t RVal None;
      (t, RVal)
  | New(ft, None) -> 
      gettype (New(ft, Some(INT(1))))
  | New(Type(t, n), Some(e)) -> 
    typecheck e (Type(Int, 0)) RVal None;
    ((Type(t, n+1), RVal))    
  | Delete(e) ->
    (try 
      let (Type(t, n), v) = gettype e in
        typecheck e (Type(Basic, 1)) RVal (Some((Type(t, n)), v));
        (Type(t, n-1), RVal)
    with 
      Match_failure _ -> (NULL, NULL))

let rec semantic node =
  match node with
  | Var_declaration(_, []) -> ()
  | Var_declaration(ft, Declarator(Id(s), ce)::t) -> 
      if ce = None then (
        symbol_push (Symbol(s, ft, None, !scope));
        semantic (Var_declaration(ft, t)))
      else (
        let Some(Const_expr(e)) = ce and Type(tp, n) = ft in
        let _ = typecheck e (Type(Int, 0)) RVal None in
        symbol_push (Symbol(s, (Type(tp, n+1)), None, !scope));
        semantic (Var_declaration(ft, t)))
  | Fun_declaration(ft, Id(s), pl) ->
      symbol_push (Symbol(s, ft, Some(pl), !scope))
  | Fun_definition(ft, Id(s), pl, dl, sl) ->
    (* TO SEE: check for previous declaration or definition *)
      symbol_push (Symbol(s, ft, Some(pl), !scope));
      scope_add ();
      let _ = List.map push_param pl in 
      ret_stack := ft :: !ret_stack;
      let _ = List.map semantic dl in
      let _ = List.map semantic sl in 
      ret_stack := List.tl !ret_stack;
      scope_delete ()
  | Empty_stmt -> ()
  | Expression(e) -> 
    let _ = typecheck e NULL NULL None in ()
  | Stmt_block(sl) -> 
    scope_add ();
    List.map semantic sl;
    scope_delete ()
  | If(e, s, None) -> 
    let _ = typecheck e (Type(Bool, 0)) RVal None in
      semantic s
  | If(e, s1, Some(s2)) -> 
    let _ = typecheck e (Type(Bool, 0)) RVal None in
      semantic s1;
      semantic s2
  | For(None, e1, e2, e3, s) ->
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> let _ = typecheck e t1 t2 None in ()
      in 
      List.map foo ([(e3, NULL, NULL); (e2, (Type(Bool, 0)), RVal); (e3, NULL, NULL)]);
      semantic s
  | For(Some(Id(id)), e1, e2, e3, s) -> 
      let foo (e, t1, t2) =
        match e with 
        | None -> ()
        | Some(e) -> let _ = typecheck e t1 t2 None in ()
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
    else let _ = typecheck (Id(s)) (Type(Label, 0)) RVal None in ()
  | Return(None) -> ()
  | Return(Some(e)) ->
      let _ = typecheck e (List.hd !ret_stack) RVal None in ()
  | _ -> raise (Semantic_Error ("Semantic was called for sth weird"))

let print_semantic_error () =
  match !semantic_errors with
  | [] -> raise End_of_semantic
  | _ ->  
    let f (s,l) =
    "Semantic Error: " ^ s ^ " at line: " ^ (string_of_int l) ^ "\n"
    in
    raise (Semantic_Error (List.fold_left (^) "" (List.map f (List.rev !semantic_errors))))

