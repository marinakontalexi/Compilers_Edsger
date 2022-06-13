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

let rec print x = 
  match x with 
  | [] -> () (*print_endline("switch to c++")*)
  | h::t -> print_declaration h; print t

and print_declaration x = 
  match x with
  | Var_declaration(ft, decl_list) -> print_endline("Variable Declaration{"); print_fulltype ft; print_declarator decl_list; print_endline("}");
  | Fun_declaration(ft, ident, param_list) -> print_endline("Function Declaration{"); print_endline("}");
  | Fun_definition(ft, ident, param_list, decl_list, stmt_list) -> print_endline("Function Definition{"); print_endline("}")

and print_fulltype ft =
  match ft with
  | Type(Int, p) -> print_endline(String.cat "Int" (string_of_int p));
  | Type(Char, p) -> print_endline(String.cat "Char" (string_of_int p));
  | Type(Bool, p) -> print_endline(String.cat "Bool" (string_of_int p));
  | Type(Double, p) -> print_endline(String.cat "Double" (string_of_int p));
  | Type(Void, p) -> print_endline(String.cat "Void" (string_of_int p))
  
and print_declarator d = (* rec *)
  match d with
  | [] -> ()
  | h::t -> print_endline("to do");
      (* let h = Declarator(ident, constant_expr) *)