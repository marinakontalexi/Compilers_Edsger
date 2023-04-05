(*Symbol Table 
  implementation: 
    -> hashtable of enrties = {id, scope}
    -> a list of entries for each scope 
  functions:
  -> st_insert
  -> st_delete
  -> st_lookup
  -> openScope
  -> closeScope
*)
open Ast

module ST_hashtbl = Hashtbl.Make (
  struct
    type t = Ast.id
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

(*entries of symbol table*)
type st_entry = {
  st_entry_id : Ast.id;
  st_entry_type : Ast.fulltype;
  st_entry_scope : scope;
  (* st_entry_info : entry_info *)
}

and scope = {
  parent_scope : scope option;
  nesting_num : int;
  mutable scope_entries : st_entry list;   
}

(* and entry_info = Variable of variable_info;

and variable_info = {
  variable_type : Ast.ft
} *)

let program_scope : scope = {
  parent_scope = None;
  nesting_num = 0;
  scope_entries = [] 
}

(*scope in use*)
let top_scope : scope ref = { contents = program_scope} 

(*construction of symbol table*)
let symbol_table = ref (ST_hashtbl.create 26) (* size?*)

let st_insert ident typ = 
    let entry : st_entry = {
      st_entry_id = ident;
      st_entry_scope = !top_scope;
      st_entry_type = typ
    } in
    ST_hashtbl.add !symbol_table ident entry;
    !top_scope.scope_entries = entry :: !top_scope.scope_entries

let st_delete entry = ST_hashtbl.remove !symbol_table entry.st_entry_id

type lookup_type = TOP_SCOPE | ALL_SCOPES

(*allagi?*)
let st_lookup ident lookup_scope_type = 
    match lookup_scope_type with
    | TOP_SCOPE -> 
        let found = ST_hashtbl.find !symbol_table ident in
        if found.st_entry_scope.nesting_num == (!top_scope).nesting_num 
        then found
        (*TO DO*)else exit 1 (*error Not found*)
    | ALL_SCOPES -> ST_hashtbl.find !symbol_table ident

let openScope = 
  let newScope : scope = {
    parent_scope = Some !top_scope;
    nesting_num = !top_scope.nesting_num + 1;
    scope_entries = []
  } in
  top_scope := newScope


let close_scope = 
    let current_scope = !top_scope in
    List.iter st_delete current_scope.scope_entries;
    match current_scope.parent_scope with
    | Some (parent_sc) -> top_scope := parent_sc
    | None -> () (*close program scope only when program finish*) 