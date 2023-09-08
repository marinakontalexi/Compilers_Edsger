open Ast

type symbol = NULL | Symbol of string * fulltype * parameter list option * int 
type 'a pointer = NULL | Pointer of 'a ref
type ilist = cell pointer
and cell = { mutable data : symbol; mutable next : ilist }

type semantic_node = Var_declaration of fulltype * declarator list
                   | Fun_declaration of fulltype * id * parameter list
                   | Fun_definition of fulltype * id * parameter list * semantic_node list * semantic_node list
                   | Empty_stmt
                   | Expression of expr
                   | Stmt_block of semantic_node list
                   | If of expr * semantic_node * semantic_node option
                   | For of id option * expr option * expr option * expr option * semantic_node
                   | Continue of id option
                   | Break of id option
                   | Return of expr option

type result_value_type = LVal | RVal | NULL

let rec stmt_to_sem (st:stmt) =
match st with
  | Empty_stmt -> (Empty_stmt: semantic_node)
  | Expression(e) -> (Expression(e): semantic_node)
  | Stmt_block(slist) -> (Stmt_block((List.map stmt_to_sem slist)): semantic_node)
  | If(e, s, None) -> (If(e, (stmt_to_sem s), None):semantic_node)
  | If(e, s1, Some(s2)) -> (If(e, (stmt_to_sem s1), Some((stmt_to_sem s2))): semantic_node)
  | For(id, e1, e2, e3, s) -> (For(id, e1, e2, e3, stmt_to_sem s): semantic_node)
  | Continue(id) -> (Continue(id): semantic_node)
  | Break(id) -> (Break(id): semantic_node)
  | Return(e) -> (Return(e): semantic_node)

let rec decl_to_sem (decl:declaration) = 
match decl with 
  | Var_declaration(ft, dlist) -> (Var_declaration(ft, dlist): semantic_node)
  | Fun_declaration(ft, id, plist) -> (Fun_declaration(ft, id, plist): semantic_node)
  | Fun_definition(ft, id, plist, dlist, slist) -> (Fun_definition(ft, id, plist, (List.map decl_to_sem dlist), (List.map stmt_to_sem slist)): semantic_node)

let ( !^ ) = function
  | Pointer r -> !r
  | NULL -> invalid_arg "Attempt to dereference the null pointer"

let ( ^:= ) p v =
  match p with
    | Pointer r -> r := v
    | NULL -> invalid_arg "Attempt to assign the null pointer"

let symbol_hash = Hashtbl.create 1234
let symbol_stack = ref []
let scope = ref 0

let symbol_push s = 
  let Symbol(id, _, _, _) = s in 
  let (prev:ilist) = 
    try
      Hashtbl.find symbol_hash (Hashtbl.hash id)
    with Not_found -> NULL 
  in
  let new_cell = {data = s; next = prev} in 
  let new_pointer = Pointer (ref new_cell)
  in
    symbol_stack := new_cell :: !symbol_stack;
    if prev != NULL then Hashtbl.remove symbol_hash (Hashtbl.hash id);
    Hashtbl.add symbol_hash (Hashtbl.hash id) new_pointer
    
let rec scope_delete () =
  match !symbol_stack with
  | [] -> ()
  | top::t -> 
    let Symbol(s, _, _, i) = top.data and next = top.next 
    in
      if i != !scope then decr scope
      else (symbol_stack := t; 
            Hashtbl.remove symbol_hash (Hashtbl.hash s);
            if next = NULL then ()
            else (Hashtbl.add symbol_hash (Hashtbl.hash s) next);
            scope_delete ())

let scope_add () = incr scope

let push_param (Param(_, ft, Id(s))) = 
  symbol_push (Symbol(s, ft, None, !scope))

let rec find_help p id =
  let c = !^p in
  let Symbol(curr, ft, pl, s) = c.data
  in
  if curr = id then Symbol(curr, ft, pl, s)
  else find_help c.next id

let symbol_find id flag =   (* flag true: global search *)
  let p =
    try 
      Hashtbl.find symbol_hash (Hashtbl.hash id)
    with Not_found -> NULL
  in
    try 
    let Symbol(curr, ft, pl, s) = find_help p id in
      if flag || (s = !scope) then Symbol(curr, ft, pl, s)
      else NULL
    with invalid_arg -> NULL
  