type un_op = AND | POINT | POS | NEG | EXC
type bin_op = TIMES | DIV | MOD | PLUS | MINUS 
            | LESS | MORE | LEQ | GEQ | EQ | NEQ
            | LOGICAL_AND | LOGICAL_OR | COMMA
type un_assign = INCR | DECR
type bin_assign = ASSIGN | TIMESEQ | DIVEQ | MODEQ | PLUSEQ | MINUSEQ
type id = Id of string
type basic_type = Int | Char | Bool | Double | Void | Label | Any
type fulltype = Type of basic_type * int | NULL(*pointer list if we use star*)
type expr = Id of string
          | True | False | NULL 
          | INT of int
          | CHAR of char
          | FLOAT of float
          | STRING of string
          | Fun_call of id * expr list
          | Table_call of expr * expr
          | Un_operation of un_op * expr
          | Bin_operation of expr * bin_op * expr
          | Un_assignment_left of un_assign * expr 
          | Un_assignment_right of expr * un_assign
          | Bin_assignment of expr * bin_assign * expr
          | Typecast of fulltype * expr
          | Question of expr * expr * expr
          | New of fulltype * expr option
          | Delete of expr
type constant_expr = Const_expr of expr
type stmt = Empty_stmt
          | Expression of expr
          | Stmt_block of stmt list
          | If of expr * stmt * stmt option
          | For of id option * expr option * expr option * expr option * stmt
          | Continue of id option
          | Break of id option
          | Return of expr option
type call = Byvalue | Byref
type parameter = Param of call * fulltype * id
type declarator = Declarator of id * constant_expr option
type declaration = Var_declaration of fulltype * declarator list
                 | Fun_declaration of fulltype * id * parameter list
                 | Fun_definition of fulltype * id * parameter list * declaration list * stmt list
type program = declaration list

let syntaxTree : program ref = ref []

let fulltype_to_string ft =
  let rec f rc n = match n with    
    | 0 -> rc    
    | p -> f ("*" ^ rc) (p-1)
  in
  match ft with
  | Type(Int, p) -> "INT" ^ (f "" p)
  | Type(Char, p) -> "CHAR" ^ (f "" p) 
  | Type(Bool, p) -> "BOOL" ^ (f "" p)  
  | Type(Double, p) -> "DOUBLE" ^ (f "" p)
  | Type(Void, p) -> "VOID" ^ (f "" p)
  | Type(Label, p) -> "Label" 
  | Type(Any, p) -> "INT, CHAR, BOOL or DOUBLE" ^ (f "" p)

let tabs = ref 0

let rec print_tabs n =
  match n with 
  | 0 -> ()
  | _ -> print_string("  "); print_tabs (n-1)

let rec print x =            (* used for printing declaration lists *)
  match x with 
  | [] -> () (*print_endline("switch to c++")*)
  | h::t -> print_declaration h; print t

and print_declaration x = 
  match x with
  | Var_declaration(ft, decl_list) -> print_tabs !tabs; print_endline("Variable Declaration {"); incr tabs;
                                      print_fulltype ft; 
                                      print_endline("Declarator List {"); incr tabs; print_declarator decl_list; decr tabs; print_tabs !tabs; print_endline("}"); 
                                      decr tabs; print_tabs !tabs; print_endline("}");
  | Fun_declaration(ft, ident, param_list) -> print_tabs !tabs; print_endline("Function Declaration {"); incr tabs;
                                              print_fulltype ft; 
                                              print_ident ident; 
                                              print_endline("Parameter List {"); incr tabs; print_param param_list; decr tabs; print_tabs !tabs; print_endline("}"); 
                                              decr tabs; print_tabs !tabs; print_endline("}"); 
  | Fun_definition(ft, ident, param_list, decl_list, stmt_list) ->  print_tabs !tabs; print_endline("Function Definition {"); incr tabs;
                                                                    print_fulltype ft; 
                                                                    print_ident ident; 
                                                                    print_endline("Parameter List {"); incr tabs; print_param param_list; decr tabs; print_tabs !tabs; print_endline("}"); 
                                                                    print_tabs !tabs; print_endline("Declaration List {"); incr tabs; print decl_list;  decr tabs; print_tabs !tabs; print_endline("} -> End Declaration List");
                                                                    print_tabs !tabs; print_endline("Statement List {"); incr tabs; print_stmt stmt_list; decr tabs; print_tabs !tabs; print_endline("}"); 
                                                                    decr tabs; print_tabs !tabs; print_endline("} -> End Function Definition"); 

and print_fulltype ft =
  match ft with
  | Type(Int, p) -> print_tabs !tabs; print_string("Type ( Int" ^ (string_of_int p) ^ " ) ");  
  | Type(Char, p) -> print_tabs !tabs; print_string("Type ( Char" ^ (string_of_int p) ^ " ) "); 
  | Type(Bool, p) -> print_tabs !tabs; print_string("Type ( Bool" ^ (string_of_int p) ^ " ) "); 
  | Type(Double, p) -> print_tabs !tabs; print_string("Type ( Double" ^ (string_of_int p) ^ " ) "); 
  | Type(Void, p) -> print_tabs !tabs; print_string("Type ( Void" ^ (string_of_int p) ^ " ) "); 
  | Type(Label, _) -> print_tabs !tabs; print_string("Type ( Label" ^ " ) "); 
  | _ -> ()
  and print_declarator d =
  match d with
  | [] -> ()
  | Declarator(ident, None)::t -> print_tabs !tabs; print_string("Declarator { ");
                                  print_ident ident; 
                                  print_endline("}");
                                  print_declarator t 
  | Declarator(ident, Some(Const_expr(e)))::t -> print_tabs !tabs; print_endline("Declarator {");
                                                 print_ident ident; 
                                                 print_expr e; 
                                                 print_endline("}");
                                                 print_declarator t

and print_ident (Id(s)) = print_string("Identifier: " ^ s ^ " ");

and print_param p = 
  match p with
  | [] -> ()
  | Param(Byvalue, ft, ident)::t ->  print_string("Parameter ( ");
                                  print_fulltype ft; 
                                  print_ident ident;
                                  print_endline(" )");
                                  print_param t;
  | Param(Byref, ft, ident)::t -> print_string("Parameter ( Byref ");
                                  print_fulltype ft; 
                                  print_ident ident;
                                  print_endline(" )");
                                  print_param t;                        

and print_stmt s = 
  match s with
  | [] -> ()
  | Empty_stmt::t ->  print_tabs !tabs; print_endline("Empty Statement"); 
                      print_stmt t;
  | Expression(e)::t -> print_expr e; 
                        print_stmt t;
  | Stmt_block(sb)::t ->  print_tabs !tabs; print_endline("Statement block {"); incr tabs;
                          print_stmt sb; 
                          decr tabs; print_tabs !tabs; print_endline("}");
                          print_stmt t;
  | If(e, s1, Some(s2))::t -> print_string("If Statement {\n If {\n"); print_expr e;
                              print_string("}\n Then {\n"); print_stmt [s1];
                              print_string("}\n Else {\n"); print_stmt [s2];
                              print_endline("}\n}");
                              print_stmt t;
  | If(e, s1, None)::t -> print_string("If Statement\nIf {\n"); print_expr e;
                          print_string("}\n Then {\n"); print_stmt [s1];
                          print_endline("}\n}");
                          print_stmt t;
  | For(None, e1, e2, e3, s)::t ->  print_tabs !tabs; print_endline("For Statement {"); incr tabs;
                                    print_expr_opt e1;
                                    print_expr_opt e2;
                                    print_expr_opt e3;
                                    print_stmt [s];
                                    decr tabs; print_tabs !tabs; print_endline("}");
                                    print_stmt t;
  | For(Some(ident), e1, e2, e3, s)::t ->   print_tabs !tabs; print_endline("For Statement {");  incr tabs;
                                            print_ident ident; print_endline("");
                                            print_expr_opt e1;
                                            print_expr_opt e2;
                                            print_expr_opt e3;
                                            print_stmt [s];
                                            decr tabs; print_endline("}"); print_tabs !tabs;
                                            print_stmt t;
  | Continue(None)::t ->  print_tabs !tabs; print_endline("Continue Statement {}");
                          print_stmt t;
  | Continue(Some(ident))::t -> print_tabs !tabs; print_string("Continue Statement {"); incr tabs;
                                print_ident ident; decr tabs; print_tabs !tabs; print_endline("}");
                                print_stmt t;
  | Break(None)::t -> print_tabs !tabs; print_endline("Break Statement {}");
                      print_stmt t;
  | Break(Some(ident))::t ->  print_tabs !tabs; print_string("Break Statement {"); incr tabs;
                              print_ident ident; decr tabs; print_tabs !tabs; print_endline("}");
                              print_stmt t;
  | Return(e)::t -> print_tabs !tabs; print_endline("Return Statement {"); incr tabs;
                  print_expr_opt e; decr tabs; print_tabs !tabs; print_endline("}");
                  print_stmt t
                              
and print_expr_opt e =
    match e with
    | None -> ()
    | Some(e) -> print_expr e

and print_expr x = 
  print_tabs !tabs; print_endline("Expression {"); incr tabs;
  match x with
  | Id(s) -> print_tabs !tabs; print_endline("Identifier " ^ s); decr tabs; print_tabs !tabs; print_endline("}");
  | True -> print_tabs !tabs; print_endline("True"); decr tabs; print_tabs !tabs; print_endline("}");
  | False -> print_tabs !tabs; print_endline("Flase"); decr tabs; print_tabs !tabs; print_endline("}");
  | NULL -> print_tabs !tabs; print_endline("NULL") ; decr tabs; print_tabs !tabs; print_endline("}");
  | INT(i) -> print_tabs !tabs; print_endline("INT: " ^ string_of_int(i)); decr tabs; print_tabs !tabs; print_endline("}");
  | CHAR(c) -> print_tabs !tabs; print_endline("CHAR: " ^ (String.make 1 c)); decr tabs; print_tabs !tabs; print_endline("}");
  | FLOAT(f) -> print_tabs !tabs; print_endline("FLOAT: " ^ string_of_float(f)); decr tabs; print_tabs !tabs; print_endline("}");
  | STRING(s) -> print_tabs !tabs; print_endline("STRING: " ^ s); decr tabs; print_tabs !tabs; print_endline("}");
  | Fun_call(id, expr_list) -> print_tabs !tabs; print_endline("Fun_call {"); incr tabs;
                               print_tabs !tabs; print_ident id;
                               print_tabs !tabs; print_endline("Expression List {"); incr tabs; print_expr_list expr_list; decr tabs; print_tabs !tabs;  print_endline("}");
                               decr tabs; print_tabs !tabs; print_endline("}");
                               decr tabs; print_tabs !tabs; print_endline("}");
  | Table_call(e1, e2) -> print_tabs !tabs; print_endline("Table_call {"); incr tabs;
                          print_expr e1; 
                          print_expr e2;
                          decr tabs; print_tabs !tabs; print_endline("}"); 
                          decr tabs; print_tabs !tabs; print_endline("}");
  | Un_operation(unOP, e) -> print_tabs !tabs; print_endline("Un_operation {"); incr tabs;
                             print_tabs !tabs; print_unOP unOP;
                             print_expr e; 
                             decr tabs; print_tabs !tabs; print_endline("}");
                             decr tabs; print_tabs !tabs; print_endline("}");
  | Bin_operation(e1, binOP, e2) -> print_tabs !tabs; print_endline("Bin_operation {"); incr tabs;
                                    print_expr e1;
                                    print_tabs !tabs; print_binOP binOP;
                                    print_expr e2; 
                                    decr tabs; print_tabs !tabs; print_endline("}");
                                    decr tabs; print_tabs !tabs; print_endline("}");
  | Un_assignment_left(unAssign, e) -> print_tabs !tabs; print_endline("Un_assignment_left {"); incr tabs;
                                       print_tabs !tabs; print_unAssign unAssign;
                                       print_expr e; 
                                       decr tabs; print_tabs !tabs; print_endline("}");
                                       decr tabs; print_tabs !tabs; print_endline("}");
  | Un_assignment_right(e, unAssign) -> print_tabs !tabs; print_endline("Un_assignment_right {"); incr tabs;
                                        print_expr e;
                                        print_tabs !tabs; print_unAssign unAssign; 
                                        decr tabs; print_tabs !tabs; print_endline("}");
                                        decr tabs; print_tabs !tabs; print_endline("}");
  | Bin_assignment(e1, binAssign, e2) -> print_tabs !tabs; print_endline("Bin_assignment {"); incr tabs;
                                         print_expr e1;
                                         print_tabs !tabs; print_binAssign binAssign;
                                         print_expr e2;
                                         decr tabs; print_tabs !tabs; print_endline("}");
                                         decr tabs; print_tabs !tabs; print_endline("}");
  | Typecast(ft, e) -> print_tabs !tabs; print_endline("Typecast {"); incr tabs;
                       print_fulltype ft;
                       print_expr e;
                       decr tabs; print_tabs !tabs; print_endline("}");
                       decr tabs; print_tabs !tabs; print_endline("}");
  | Question(e1, e2, e3) -> print_tabs !tabs; print_endline("Question {"); incr tabs;
                            print_expr(e1);
                            print_expr(e2);
                            print_expr(e3);
                            decr tabs; print_tabs !tabs; print_endline("}");
                            decr tabs; print_tabs !tabs; print_endline("}");
  | New(ft, e) -> print_tabs !tabs; print_endline("New {"); incr tabs;
                  print_fulltype ft;
                  print_expr_opt e;
                  decr tabs; print_tabs !tabs; print_endline("}");
                  decr tabs; print_tabs !tabs; print_endline("}");
  | Delete(e) -> print_tabs !tabs; print_endline("Delete {"); incr tabs;
                 print_expr e;
                 decr tabs; print_tabs !tabs; print_endline("}");
                 decr tabs; print_tabs !tabs; print_endline("}");

and print_expr_list expr_list =
  match expr_list with
  | [] -> ()
  | h::t -> print_expr h;
            print_expr_list t;

and print_unOP op = 
  match op with
  | AND -> print_string(" & ")
  | POINT -> print_string(" * ")
  | POS -> print_string(" + ")
  | NEG -> print_string(" - ")
  | EXC -> print_string(" ! ")

and print_binOP op = 
  match op with
  | TIMES -> print_string(" * ")
  | DIV -> print_string(" / ")
  | MOD -> print_string(" % ")
  | PLUS -> print_string(" + ")
  | MINUS -> print_string(" - ")
  | LESS -> print_string(" < ")
  | MORE -> print_string(" > ")
  | LEQ -> print_string(" <= ")
  | GEQ -> print_string(" >= ")
  | EQ -> print_string(" == ")
  | NEQ -> print_string(" != ")
  | LOGICAL_AND -> print_string(" && ")
  | LOGICAL_OR -> print_string(" || ")
  | COMMA -> print_string(" , ")

and print_binAssign op = 
  match op with
  | ASSIGN -> print_string(" = ")
  | TIMESEQ -> print_string(" *= ")
  | DIVEQ -> print_string(" /= ")
  | MODEQ -> print_string(" %= ")
  | PLUSEQ -> print_string(" += ")
  | MINUSEQ -> print_string(" -= ")

  and print_unAssign op = 
  match op with
  | INCR -> print_string(" ++ ")
  | DECR -> print_string(" -- ")