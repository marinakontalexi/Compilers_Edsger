type un_op = AND | POINT | POS | NEG | EXC
type bin_op = TIMES | DIV | MOD | PLUS | MINUS 
            | LESS | MORE | LEQ | GEQ | EQ | NEQ
            | LOGICAL_AND | LOGICAL_OR | COMMA
type un_assign = INCR | DECR
type bin_assign = ASSIGN | TIMESEQ | DIVEQ | MODEQ | PLUSEQ | MINUSEQ
type id = Id of string
type basic_type = Int | Char | Bool | Double | Void
type fulltype = Type of basic_type * int (*pointer list if we use star*)
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
type call = None | Byref
type parameter = Param of call * fulltype * id
type declarator = Declarator of id * constant_expr option
type declaration = Var_declaration of fulltype * declarator list
                 | Fun_declaration of fulltype * id * parameter list
                 | Fun_definition of fulltype * id * parameter list * declaration list * stmt list
type program = declaration list

let syntaxTree : program ref = ref []

let rec print x =            (* used for printing declaration lists *)
  match x with 
  | [] -> () (*print_endline("switch to c++")*)
  | h::t -> print_declaration h; print t

and print_declaration x = 
  match x with
  | Var_declaration(ft, decl_list) -> print_endline("Variable Declaration {"); 
                                      print_fulltype ft; 
                                      print_endline("Declarator List {"); print_declarator decl_list; print_endline("}");
                                      print_endline("}");
  | Fun_declaration(ft, ident, param_list) -> print_endline("Function Declaration {"); 
                                              print_fulltype ft; 
                                              print_ident ident; 
                                              print_endline("Parameter List {"); print_param param_list; print_endline("}");
                                              print_endline("}");
  | Fun_definition(ft, ident, param_list, decl_list, stmt_list) ->  print_endline("Function Definition {"); 
                                                                    print_fulltype ft; 
                                                                    print_ident ident; 
                                                                    print_endline("Parameter List {"); print_param param_list; print_endline("}");
                                                                    print_endline("Declaration List {"); print decl_list; print_endline("}");
                                                                    print_endline("Statement List {"); print_stmt stmt_list; print_endline("}");
                                                                    print_endline("}")

and print_fulltype ft =
  match ft with
  | Type(Int, p) ->  print_string("Type ( Int" ^ (string_of_int p) ^ " ) ");  
  | Type(Char, p) -> print_string("Type ( Char" ^ (string_of_int p) ^ " ) "); 
  | Type(Bool, p) -> print_string("Type ( Bool" ^ (string_of_int p) ^ " ) "); 
  | Type(Double, p) -> print_string("Type ( Double" ^ (string_of_int p) ^ " ) "); 
  | Type(Void, p) -> print_string("Type ( Void" ^ (string_of_int p) ^ " ) "); 
  
and print_declarator d =
  match d with
  | [] -> ()
  | Declarator(ident, None)::t -> print_string("Declarator { ");
                                  print_ident ident; 
                                  print_endline("}");
                                  print_declarator t 
  | Declarator(ident, Some(Const_expr(e)))::t -> print_endline("Declarator {");
                                                 print_ident ident; 
                                                 print_expr e; 
                                                 print_endline("}");
                                                 print_declarator t

and print_ident (Id(s)) = print_string("Identifier: " ^ s ^ " ");

and print_param p = 
  match p with
  | [] -> ()
  | Param(None, ft, ident)::t ->  print_string("Parameter ( ");
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
  | Empty_stmt::t ->  print_endline("Empty Statement"); 
                      print_stmt t;
  | Expression(e)::t -> print_expr e; 
                        print_stmt t;
  | Stmt_block(sb)::t ->  print_endline("Statement block {"); 
                          print_stmt sb; 
                          print_endline("}");
                          print_stmt t;
  | If(e, s1, Some(s2))::t -> print_string("If Statement {\n If {\n"); print_expr e;
                              print_string("}\n Then {\n"); print_stmt [s1];
                              print_string("}\n Else {\n"); print_stmt [s2];
                              print_endline("}\n}");
                              print_stmt t;
  | If(e, s1, None)::t -> print_string("If Statement\n If {\n"); print_expr e;
                          print_string("}\n Then {\n"); print_stmt [s1];
                          print_endline("}\n}");
                          print_stmt t;
  | For(None, e1, e2, e3, s)::t ->  print_endline("For Statement {"); 
                                    print_expr_opt e1;
                                    print_expr_opt e2;
                                    print_expr_opt e3;
                                    print_stmt [s];
                                    print_endline("}");
                                    print_stmt t;
  | For(Some(ident), e1, e2, e3, s)::t ->   print_endline("For Statement {"); 
                                            print_ident ident; print_endline("");
                                            print_expr_opt e1;
                                            print_expr_opt e2;
                                            print_expr_opt e3;
                                            print_stmt [s];
                                            print_endline("}");
                                            print_stmt t;
  | Continue(None)::t ->  print_endline("Continue Statement {}");
                          print_stmt t;
  | Continue(Some(ident))::t -> print_string("Continue Statement {");
                                print_ident ident; print_endline("}");
                                print_stmt t;
  | Break(None)::t -> print_endline("Break Statement {}");
                      print_stmt t;
  | Break(Some(ident))::t ->  print_string("Break Statement {");
                              print_ident ident; print_endline("}");
                              print_stmt t;
  | Return(e)::t -> print_endline("Return Statement {");
                  print_expr_opt e; print_endline("}");
                  print_stmt t
                              
and print_expr_opt e =
    match e with
    | None -> ()
    | Some(e) -> print_expr e

and print_expr x = 
  print_endline(" Expression {");
  match x with
  | Id(s) -> print_endline("String " ^ s); print_endline("}");
  | True -> print_endline("True"); print_endline("}");
  | False -> print_endline("Flase"); print_endline("}");
  | NULL -> print_endline("NULL") ; print_endline("}");
  | INT(i) -> print_endline("INT: " ^ string_of_int(i)); print_endline("}");
  | CHAR(c) -> print_endline("CHAR: " ^ (String.make 1 c)); print_endline("}");
  | FLOAT(f) -> print_endline("FLOAT: " ^ string_of_float(f)); print_endline("}");
  | STRING(s) -> print_endline("STRING: " ^ s); print_endline("}");
  | Fun_call(id, expr_list) -> print_endline("Fun_call {");
                               print_ident id;
                               print_endline("Expression List {"); print_expr_list expr_list; print_endline("}");
                               print_endline("}");
                               print_endline("}");
  | Table_call(e1, e2) -> print_endline("Table_call {");
                          print_expr e1; 
                          print_expr e2;
                          print_endline("}"); 
                          print_endline("}");
  | Un_operation(unOP, e) -> print_endline("Un_operation {");
                             print_unOP unOP;
                             print_expr e; 
                             print_endline("}");
                             print_endline("}");
  | Bin_operation(e1, binOP, e2) -> print_endline("Bin_operation {");
                                    print_expr e1;
                                    print_binOP binOP;
                                    print_expr e2; 
                                    print_endline("}");
                                    print_endline("}");
  | Un_assignment_left(unAssign, e) -> print_endline("Un_assignment_left {");
                                       print_unAssign unAssign;
                                       print_expr e; 
                                       print_endline("}");
                                       print_endline("}");
  | Un_assignment_right(e, unAssign) -> print_endline("Un_assignment_right {");
                                        print_expr e;
                                        print_unAssign unAssign; 
                                        print_endline("}");
                                        print_endline("}");
  | Bin_assignment(e1, binAssign, e2) -> print_endline("Bin_assignment {");
                                         print_expr e1;
                                         print_binAssign binAssign;
                                         print_expr e2;
                                         print_endline("}");
                                         print_endline("}");
  | Typecast(ft, e) -> print_endline("Typecast {");
                       print_fulltype ft;
                       print_expr e;
                       print_endline("}");
                       print_endline("}");
  | Question(e1, e2, e3) -> print_endline("Question {");
                            print_expr(e1);
                            print_expr(e2);
                            print_expr(e3);
                            print_endline("}");
                            print_endline("}");
  | New(ft, e) -> print_endline("New {");
                  print_fulltype ft;
                  print_expr_opt e;
                  print_endline("}");
                  print_endline("}");
  | Delete(e) -> print_endline("Delete {");
                 print_expr e;
                 print_endline("}");
                 print_endline("}");

and print_expr_list expr_list =
  match expr_list with
  | [] -> ()
  | h::t -> print_expr h;
            print_expr_list t;

and print_unOP op = print_endline("OPERATOR");

and print_binOP op = print_endline("OPERATOR");

and print_unAssign op = print_endline("OPERATOR");

and print_binAssign op = print_endline("OPERATOR");