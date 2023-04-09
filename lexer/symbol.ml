(* open Ast *)

type symbol = NULL | Symbol of string * fulltype * parameter list option * int 
type 'a pointer = NULL | Pointer of 'a ref
type ilist = cell pointer
and cell = { mutable data : symbol; mutable next : ilist }

let ( !^ ) = function
  | Pointer r -> !r
  | NULL -> invalid_arg "Attempt to dereference the null pointer"

let ( ^:= ) p v =
  match p with
    | Pointer r -> r := v
    | NULL -> invalid_arg "Attempt to assign the null pointer"

let symbol_hash = Hashtbl.create 1234
let symbol_stack = ref []
let ret_stack = ref []
let scope = ref 0

let symbol_push s = 
  let Symbol(id, _, _, _) = s in 
  let prev = 
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
    with Not_found -> print_endline("Not found"); NULL
  in
    try 
    let Symbol(curr, ft, pl, s) = find_help p id in
      if flag or (s = !scope) then Symbol(curr, ft, pl, s)
      else NULL
    with invalid_arg -> print_endline("Invalid"); NULL
  