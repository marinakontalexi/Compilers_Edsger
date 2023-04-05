open Ast
open Symbol

type semantic_node = program | declaration | declarator
type result_value_type = LVal | RVal
(* type semantic_node = Program of declaration list *)

(* let type_find (Param (_, ft, _)) = ft
let param_find param_list = List.map type_find param_list *)

let rec declaration_push decl =
  match decl with
  | Parameter_definition([]) -> ()
  | Parameter_definition(Param(_, ft, ident)::t) -> let d = Declarator(ident, None) in
                                                    declaration_push (Var_declaration(ft, [d]));
                                                    declaration_push (Parameter_definition(t))           
  | Var_declaration (_, []) -> ()
  | Var_declaration(Type(a,x), Declarator(Id(s), e)::t) ->if e == None 
                                                          then 
                                                            let symbol = (s, Type(a,x),!scope, None) in
                                                              symbol_push symbol;
                                                              declaration_push (Var_declaration(Type(a,x), t))
                                                          else  
                                                            let symbol = (s, Type(a, x+1),!scope, None) in
                                                              symbol_push symbol;
                                                              declaration_push (Var_declaration(Type(a,x), t))
  | Fun_declaration(ft, Id(s), pl) -> let symbol = (s, ft, !scope, Some(pl)) in 
                                      symbol_push symbol
  | Fun_definition(ft, Id(s), [], dl, _) -> ()                                    
  | Fun_definition(ft, Id(s), pl, dl, sl) -> declaration_push (Fun_declaration(ft, Id(s), pl));
                                            incr scope;
                                            declaration_push (Parameter_definition(pl));
                                            declaration_push (Decl_list(dl));
                                            (* check (Stmt_block(sl)) *)
                                            scope_delete;
                                            decr scope
  | Decl_list([]) -> ()
  | Decl_list(a::t) -> 
      (match a with
      | Var_declaration(ft, dl) -> declaration_push (Var_declaration(ft, dl)); declaration_push (Decl_list(t))
      | Fun_declaration(ft, ident, pl) -> declaration_push (Fun_declaration(ft, ident, pl)); declaration_push (Decl_list(t))
      | Fun_definition(ft, ident, pl, dl, sl) -> declaration_push (Fun_definition(ft, ident, pl, dl, sl)); declaration_push (Decl_list(t))           
      )

let semantic ast =
  match ast with
  | Declaration_List([]) -> print_endline("Success!")
  | Declaration_List(h::t) -> semantic h; semantic Declaration_List(t)
  | Var_declaration(_, []) -> ()
  | Var_declaration(ft, h::t) -> 
    let Declarator(Id(s), ce) = h in
      if ce == Some(e) then (
        check e Type(Int, 0) RVal
      ); 
      symbol_push Symbol(s, ft, [], !scope);
      semantic Var_declaration(ft, t)
  | Fun_declaration(ft, Id(s), pl) ->
  | Fun_definition() ->
  
let check s =
  match s with
  | None -> ()
  | Empty_stmt -> ()
  | Expression(e) -> scope_check e
  | Stmt_block (h::t) -> check h; check Stmt_block(t)
  | If(e, s1, None) -> scope_check e; if ((typecheck e) == Type(Bool, 0)) then (check s1) (* else raise exc *)
  | If(e, s1, Some(s2)) -> if (typecheck e) == Type(Bool, 0) then (check s1; check s2) (* else raise exc *)  
  | For(None, e1, e2, e3, s) -> scope_check e1; scope_check e3; if ((typecheck e2) == Type(Bool, 0) or e2 == None) then check s
  |
