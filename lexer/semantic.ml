let scope_hash = Hashtbl.create 1234
let symbol_stack = ref []
let scope = ref 0

let scope_add = incr scope

let scope_delete = 
  if !scope == 0 then ()
  else 
    let rec loop =
      match !symbol_stack with
      | [] -> ()
      | (_, _, !scope)::t -> symbol_stack := t; loop;
      | (_, _, _)::t -> ()
    in 
      loop; decr scope
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
