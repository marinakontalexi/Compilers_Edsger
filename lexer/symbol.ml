type 'a pointer = NULL | Pointer of 'a ref

let ( !^ ) = function
  | NULL -> invalid_arg "Attempt to dereference the null pointer"
  | Pointer r -> !r;

let ( ^:= ) p v =
  match p with
    | NULL -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v;

type symbol = Symbol of string * fulltype * symbol list * int
type ilist = cell pointer
and cell = { mutable data : symbol; mutable next : ilist }

let symbol_hash = Hashtbl.create 1234
let symbol_stack = ref []
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