open Ast
open Symbol

type semantic_node  = Program of program
                    | Declaration_sem of declaration
                    | Stmt_sem of stmt
                    | Declaration_list_sem of declaration list (*xreiazete?? einai idio me to program*) 
                    | Stmt_list_sem of stmt list


let type_check a = ()
(* let st_insert a = () *)
(* let open_scope = () *)
(* let close_scope = () *)
let st_insert_pl pl = ()
let check_label_exists label= ()

let type_equal ft1 ft2 = 
    if(ft1 != None && ft2 != None) then
        let Some(Type(bt1, p1)) = ft1
        and  Some(Type(bt2, p2)) = ft2 in
            (bt1 == bt2) && (p1 == p2)
    else
        (ft1 == None) && (ft2 == None)

(*theloume na einai ref i na girnane i sinartiseiw piso kati*)
let ret_type : fulltype option ref = {contents = None}
let function_ret_type : fulltype option ref = {contents = None}
let inLoop : bool ref = {contents = false}


type result_value_type = LVal | RVal
(* check_expr e -> (fulltype option, result_value_type) *)
let rec check_expr e = 
    let ft (bt,p) = Some(Type(bt, p)) in
    match e with
    | Id ident ->
        (*kanonika na einai mesa se try ... with Not_Found*)
        let entry = st_lookup (Id ident) ALL_SCOPES in (entry.st_entry_type, LVal)
    | True -> (ft(Bool, 0), RVal)
    | False -> (ft(Bool, 0), RVal)
    | NULL -> (ft(Void, -1), RVal) (*eidiki periptosi (Void,-1) = NULL *)
    | INT i -> (ft(Int, 0), RVal)
    | CHAR c -> (ft(Char, 0), RVal)
    | FLOAT f -> (ft(Double, 0), RVal)
    | STRING s -> (ft(Char, 1), RVal)
    | Fun_call (id, el) -> ()
    | Table_call (e1, e2) -> ()
    | Un_operation (op1, e) -> ()
    | Bin_operation (e1, op, e2) -> ()
    | Un_assignment_left (ua, e) -> ()
    | Un_assignment_right (e, ua) -> ()
    | Bin_assignment (e1, ba, e2) -> ()
    | Typecast (ft, e) -> ()
    | Question (e1,e2,e3) -> ()
    | New (ft, e_option) -> ()
    | Delete e -> ()

let rec sem node =
  match node with
  | Program([]) -> 
      print_endline("Success!")
  | Program(h::t) -> 
      sem (Declaration_sem(h)); 
      sem (Program(t))
  
  | Declaration_list_sem([]) -> ()
  | Declaration_list_sem(h::t) ->
      sem (Declaration_sem(h));
      sem (Declaration_list_sem(t))
  | Stmt_list_sem([]) -> ()
  | Stmt_list_sem(h::t) ->
      sem (Stmt_sem(h));
      sem (Stmt_list_sem(t))

  | Declaration_sem(Var_declaration(ft,dl))->(*dl = declarator_list*)
      if (dl == []) then ()
      else(
        let h::t = dl
        in(
          let Declarator(ident, ce) = h
            in(  
            if (ce == SOME(e)) then (
                let (typ, val_info) = check_expr e in
                if (typ != Type(Int, 0) or val_info != RVal) then (*to do*)exit 1
            );
            st_insert(ident, ft);
            sem (Declaration_sem(Var_declaration(ft, t)))
            )
          )
      )
  | Declaration_sem(Fun_declaration(ft,ident, pl))->
      st_insert(ident, ft);
      (*check if there is another function with the same name and parameters*)
  | Declaration_sem(Fun_definition(ft, ident, pl, dl, sl))->
      (*??check if the return value is the same as the ft*)
      open_scope;
      st_insert_pl(pl);
      sem (Declaration_list_sem(dl));
      sem (Stmt_list_sem(sl));
      close_scope
  
  | Stmt_sem(Empty_stmt)->()
  | Stmt_sem(Expression(e))->
      check_expr(e)
  | Stmt_sem(Stmt_block(sl))->
    (*xreiazete na anoiksei mipos kanourgio scope?*)
      sem (Stmt_list_sem(sl))
  | Stmt_sem(If(e, s, None))->
    (*xreiazete na anoiksei mipos kanourgio scope?*)
      type_check(e, Type(Bool,0));
      sem (Stmt_sem(s))
  | Stmt_sem(If(e, s1, Some(s2)))->
      type_check(e, Type(Bool,0));
      sem (Stmt_sem(s1));
      let temp_type = !ret_type in
        sem (Stmt_sem(s2));
        (*check that s1 s2 gives the same type*)
        if not (type_equal temp_type !ret_type) then (*error*) () 
  | Stmt_sem(For(ident, e1, e2, e3, s))->()
        (*xreiazete na anoiksei mipos kanourgio scope?*)
  | Stmt_sem(Continue(None))->
      (* check if Continue is inside a loop *)
        if(!inLoop == false) then () (*error*)
  | Stmt_sem(Continue(Some(label)))->
        if(!inLoop == false) then () (*error*)
        else (check_label_exists label)
  | Stmt_sem(Break(None))->
      (* check if Break is inside a loop *)
        if(!inLoop == false) then () (*error*)
  | Stmt_sem(Break(Some(label)))->
        if(!inLoop == false) then () (*error*)
        else (check_label_exists label)
  | Stmt_sem(Return(None))->
      (* check that function type is void *)
      if not (type_equal (!function_ret_type) (Some(Type(Void, 0)))) then () (*error*)
  | Stmt_sem(Return(Some(e)))->
      (* check that function type is equal with e *)
      check_expr e;
      if not (type_equal (!function_ret_type) (!ret_type)) then () (*error*)

