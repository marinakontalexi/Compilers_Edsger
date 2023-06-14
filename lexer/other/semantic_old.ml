open Ast

type symbol_node = Var_declaration of fulltype * declarator list
                 | Fun_declaration of fulltype * id * parameter list
                 | Fun_definition of fulltype * id * parameter list * declaration list * stmt list
                 | Parameter_definition of parameter list
                 | Decl_list of declaration list

type semantic_node = Program of declaration list

let scope_hash = Hashtbl.create 1234
let symbol_stack = ref []
let scope = ref 0

let rec loop stack =
  match stack with
  | [] -> ()
  | (s, _, i, _)::t -> if i == !scope then symbol_stack := t; 
                                           Hashtbl.remove scope_hash(Hashtbl.hash s);
                                           loop !symbol_stack

let scope_delete = 
  if !scope == 0 then ()
  else loop !symbol_stack

let symbol_push symbol = 
  let (s, _, _, _) = symbol in
    Hashtbl.add scope_hash (Hashtbl.hash s) symbol;
    symbol_stack := symbol :: !symbol_stack

let type_find (Param (_, ft, _)) = ft
let param_find param_list = List.map type_find param_list

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

(* let semantic ast =
  match ast with
  | Program([]) -> print_endline("Success!")
  | Program(h::t) -> semantic Declaration_sem(h); semantic Program(t)
  | Declaration_sem(Var_declaration(ft,dl))->
  | Declaration_sem(Fun_declaration()) *)
  
let check s =
  match s with
  | Empty_stmt -> ()
  | Expression(e) -> scope_check e
  | Stmt_block (h::t) -> check h; check Stmt_block(t)
  | If(e, s1, None) -> scope_check e; if ((typecheck e) == Type(Bool, 0)) then (check s1) (* else raise exc *)
  | If(e, s1, Some(s2)) -> if (typecheck e) == Type(Bool, 0) then (check s1; check s2) (* else raise exc *)  
  | For(None, e1, e2, e3, s) -> scope_check e1; scope_check e3; if ((typecheck e2) == Type(Bool, 0) or e2 == None) then check s
  |