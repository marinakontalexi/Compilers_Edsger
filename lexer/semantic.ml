open Ast

type symbol_node = Var_declaration of fulltype * declarator list
                 | Fun_declaration of fulltype * id * parameter list
                 | Parameter_definition of parameter list
                 | Decl_list of declaration list

let scope_hash = Hashtbl.create 1234
let symbol_stack = ref []
let scope = ref 0

let scope_add = incr scope

let scope_delete = 
  if !scope == 0 then ()
  else 
    let rec loop =
      match (!symbol_stack) with
      | [] -> ()
      | (s, _, (!scope), _)::t -> symbol_stack := t; 
                                Hashtbl.remove scope_hash(Hashtbl.hash s);
                                loop;
      | _::t -> ()
    in 
      loop;

let symbol_push symbol = 
  let (s, _, _, _) = symbol in
    Hashtbl.add scope_hash (Hashtbl.hash s) symbol;
    symbol_stack := symbol :: !symbol_stack

let type_find Param(_, ft, _) = ft
let param_find param_list = List.map type_find param_list


let rec declaration_push decl =
  match decl with
  | Decl_list([]) -> ()
  | Decl_list(a::t) -> declaration_push a; declaration_push Decl_list(t)
  | Var_declaration (_, []) -> ()
  | Var_declaration(Type(a,x), Declarator(Id(s), e)::t) ->if e == None 
                                                          then 
                                                            let symbol = (s, Type(a,x),!scope, None) in
                                                              symbol_push symbol;
                                                              declaration_push Var_declaration(Type(a,x), t)
                                                          else  
                                                            let symbol = (s, Type(a, x+1),!scope, None) in
                                                              symbol_push symbol;
                                                              declaration_push Var_declaration(Type(a,x), t)
  | Fun_declaration(ft, Id(s), pl) -> let symbol = (s, ft, !scope, Some(param_find pl)) in 
                                      symbol_push symbol;
  | Fun_definition(ft, Id(s), [], dl, _) -> ()                                    
  | Fun_definition(ft, Id(s), pl, dl, _) -> declaration_push(Fun_declaration(ft, Id(s), pl));
                                            incr scope;
                                            declaration_push Parameter_definition(pl);
                                            declaration_push Decl_list(dl);
                                            scope_delete;
                                            decr scope                      
  | Parameter_definition([]) -> ()
  | Parameter_definition(Param(_, ft, ident)::t) -> let d = Declarator(ident, None) in
                                                    declaration_push(Var_declaration(ft, [d]));
                                                    declaration_push Parameter_definition(t)



(* 
let scopes program = 
  match program with 
  | [] -> ()
  | Var_declaration(var_type, d_list)::t -> 
  | Fun_definition()::t ->
  | Fun_declaration::t ->

let main () =
  Hashtbl.add scope_hash "a" "giorgos";
  if Hashtbl.mem scope_hash (Hashtbl.hash "giorgos") then print_endline("yes") else print_endline("NO") *)
