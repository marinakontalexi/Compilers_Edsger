open Ast

type 'a pointer = NULL | Pointer of 'a ref
type symbol = Symbol of string * fulltype * parameter list option * int
type ilist = cell pointer
and cell = { mutable data : symbol; mutable next : ilist }

let ( !^ ) = function
  | NULL -> invalid_arg "Attempt to dereference the null pointer"
  | Pointer r -> !r

let ( ^:= ) p v =
  match p with
    | NULL -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v

let symbol_hash = Hashtbl.create 1234
let symbol_stack = ref []
let ret_stack = ref []
let scope = ref 0

let symbol_push s = 
  let Symbol(id, _, _, _) = s 
  and prev = 
    try
      Hashtbl.find symbol_hash (Hashtbl.hash id)
    with Not_found -> NULL
  and new_cell = {data = s; next = prev} 
  and new_pointer = Pointer (ref new_cell)
  in
    symbol_stack := new_cell :: !symbol_stack;
    if prev != NULL then Hashtbl.remove symbol_hash (Hashtbl.hash id);
    Hashtbl.add symbol_hash (Hashtbl.hash id) new_pointer
    
let rec scope_delete =
  match !symbol_stack with
  | [] -> ()
  | top::t -> 
    let (s, _, _, i) = top.data and next = top.next 
    in
      if i != !scope then decr scope
      else (symbol_stack := t; 
            Hashtbl.remove symbol_hash (Hashtbl.hash s);
            if next == NULL then ()
            else (Hashtbl.add symbol_hash (Hashtbl.hash s) next);
            scope_delete ())

let scope_add = incr scope

let push_param Param(_, ft, Id(s)) = 
  symbol_push Symbol(s, ft, None, !scope)

let symbol_find id s =         (* here *)
  let p =
    try 
      Hashtbl.find symbol_hash (Hashtbl.hash id)
    with Not_found -> NULL
  and cell = 
    try !^p
    with invalid_arg -> NULL
  in


