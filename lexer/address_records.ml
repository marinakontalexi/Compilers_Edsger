open Llvm

type var = Var of string * llvalue * int
type 'a pointer = NULL | Pointer of 'a ref
type ilist = cell pointer
and cell = { mutable data : var; mutable next : ilist }

let ( !^ ) = function
  | Pointer r -> !r
  | NULL -> invalid_arg "Attempt to dereference the null pointer"

let ( ^:= ) p v =
  match p with
    | Pointer r -> r := v
    | NULL -> invalid_arg "Attempt to assign the null pointer"

let variable_hash = Hashtbl.create 1234
let variable_stack = ref []
let scope = ref 0

let variable_push s alloca = 
  let (prev:ilist) = 
    try
      Hashtbl.find variable_hash (Hashtbl.hash s)
    with Not_found -> NULL 
  in
  let new_cell = {data = (Var(s, alloca, !scope)); next = prev} in 
  let new_pointer = Pointer (ref new_cell)
  in
    variable_stack := new_cell :: !variable_stack;
    if prev != NULL then Hashtbl.remove variable_hash (Hashtbl.hash s);
    Hashtbl.add variable_hash (Hashtbl.hash s) new_pointer
    
let rec function_delete () =
  match !variable_stack with
  | [] -> ()
  | top::t -> 
    let Var(s, _, i) = top.data and next = top.next 
    in
      if i != !scope then decr scope
      else (variable_stack := t; 
            Hashtbl.remove variable_hash (Hashtbl.hash s);
            if next = NULL then ()
            else (Hashtbl.add variable_hash (Hashtbl.hash s) next);
            function_delete ())

let function_add () = incr scope

let rec find_help p id =
  let c = !^p in
  let Var(curr, alloca, s) = c.data
  in
  if curr = id then Var(curr, alloca, s)
  else find_help c.next id

let variable_find id = 
  let p =
    try 
      Hashtbl.find variable_hash (Hashtbl.hash id)
    with Not_found -> NULL
  in
    try find_help p id
    with _ -> invalid_arg "error in variable_find"
  