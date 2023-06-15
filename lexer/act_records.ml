open Ast
open Llvm

type var = Var of string * llvalue
type 'a pointer = NULL | Pointer of 'a ref
type ilist = cell pointer
and cell = { mutable data : var; mutable next : ilist }
type alist = act pointer
and act = { mutable record : (int, cell pointer) Hashtbl.t; mutable next : alist; mutable stack : cell list ref}


let ( !^ ) = function
  | Pointer r -> !r
  | NULL -> invalid_arg "Attempt to dereference the null pointer"

let ( ^:= ) p v =
  match p with
    | Pointer r -> r := v
    | NULL -> invalid_arg "Attempt to assign the null pointer"


let act_list = NULL : alist
let current_act = NULL : alist

let create_activation_record () = 
  let variable_hash = Hashtbl.create 1234 in
  let variable_stack = ref [] in
  if act_list = NULL then 
    act_list = 
    



let variable_push (Var(id, lv)) = 
  let (prev:ilist) = 
    try
      Hashtbl.find variable_hash (Hashtbl.hash id)
    with Not_found -> NULL 
  in
  let new_cell = {data = (Var(id, lv)); next = prev} in 
  let new_pointer = Pointer (ref new_cell)
  in
    variable_stack := new_cell :: !variable_stack;
    if prev != NULL then Hashtbl.remove variable_hash (Hashtbl.hash id);
    Hashtbl.add variable_hash (Hashtbl.hash id) new_pointer
    
let rec scope_delete () =
  match !variable_stack with
  | [] -> ()
  | top::t -> 
    let Symbol(s, _, _, i) = top.data and next = top.next 
    in
      if i != !scope then decr scope
      else (variable_stack := t; 
            Hashtbl.remove variable_hash (Hashtbl.hash s);
            if next = NULL then ()
            else (Hashtbl.add variable_hash (Hashtbl.hash s) next);
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
      Hashtbl.find variable_hash (Hashtbl.hash id)
    with Not_found -> NULL
  in
    try 
    let Symbol(curr, ft, pl, s) = find_help p id in
      if flag || (s = !scope) then Symbol(curr, ft, pl, s)
      else NULL
    with invalid_arg -> NULL
  