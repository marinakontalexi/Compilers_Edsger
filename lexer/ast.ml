type un_op = AND | POINT | POS | NEG | EXC
type bin_op = TIMES | DIV | MOD | PLUS | MINUS 
            | LESS | MORE | LEQ | GEQ | EQ | NEQ
            | LOGICAL_AND | LOGICAL_OR | COMMA
type un_assign = INCR | DECR
type bin_assign = ASSIGN | TIMESEQ | DIVEQ | MODEQ | PLUSEQ | MINUSEQ
type id = Id of string
type basic_type = Int | Char | Bool | Double | Void | Label | Basic | BV | Num
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

exception End_of_parser of program

let unop_to_string op = 
  match op with
  | AND -> " & "
  | POINT -> " * "
  | POS -> " + "
  | NEG -> " - "
  | EXC -> " ! "

and binop_to_string op = 
  match op with
  | TIMES -> " * "
  | DIV -> " / "
  | MOD -> " % "
  | PLUS -> " + "
  | MINUS -> " - "
  | LESS -> (" < ")
  | MORE -> (" > ")
  | LEQ -> (" <= ")
  | GEQ -> (" >= ")
  | EQ -> (" == ")
  | NEQ -> (" != ")
  | LOGICAL_AND -> (" && ")
  | LOGICAL_OR -> " || "
  | COMMA -> " , "

and binass_to_string op = 
  match op with
  | ASSIGN -> " = "
  | TIMESEQ -> " *= "
  | DIVEQ -> " /= "
  | MODEQ -> " %= "
  | PLUSEQ -> " += "
  | MINUSEQ -> " -= "

  and unass_to_string op = 
  match op with
  | INCR -> " ++ "
  | DECR -> " -- "

let fulltype_to_string ft =
  let rec f rc n = match n with    
    | 0 -> rc    
    | p -> 
      if p < 0 then "^"
      else f ("*" ^ rc) (p-1)
  in
  match ft with
  | Type(Int, p) -> "INT" ^ (f "" p)
  | Type(Char, p) -> "CHAR" ^ (f "" p) 
  | Type(Bool, p) -> "BOOL" ^ (f "" p)  
  | Type(Double, p) -> "DOUBLE" ^ (f "" p)
  | Type(Void, p) -> "VOID" ^ (f "" p)
  | Type(Label, p) -> "Label" 
  | Type(Basic, p) -> let star = f "" p in "INT" ^ star ^ ", CHAR" ^ star ^ ", BOOL" ^ star ^ " or DOUBLE" ^ star 
  | Type(BV, p) -> let star = f "" p in "INT" ^ star ^ ", CHAR" ^ star ^ ", BOOL" ^ star ^ ", DOUBLE" ^ star ^ " or VOID" ^ star
  | Type(Num, p) -> "INT or DOUBLE" ^ (f "" p)
  | _ -> "?"

let rec expr_to_string (expression:expr) = 
  match expression with
| NULL -> ""
| Id(s) | STRING(s) -> s
| True -> "true" 
| False -> "false"
| INT(i) -> string_of_int i
| CHAR(c) -> "\'" ^ (String.make 1 c) ^ "\'"
| FLOAT(f) -> string_of_float f
| Fun_call(Id(s), elist) -> 
  if elist = [] then s ^ "()" 
  else 
    let h = expr_to_string (List.hd elist) in
    let foo s = ", " ^ s in  
      s ^ "(" ^ (List.fold_left (^) h (List.map foo (List.map expr_to_string (List.tl elist)))) ^ ")"

| Table_call(e1, e2) -> expr_to_string e1 ^ "[" ^ expr_to_string e2 ^ "]"

| Un_operation(op, e) -> unop_to_string op ^ expr_to_string e
| Bin_operation(e1, op, e2) -> expr_to_string e1 ^ binop_to_string op ^ expr_to_string e2

| Un_assignment_left(op, e) -> unass_to_string op ^ expr_to_string e
| Un_assignment_right(e, op) -> expr_to_string e ^ unass_to_string op

| Bin_assignment(e1, op, e2) -> expr_to_string e1 ^ binass_to_string op ^ expr_to_string e2

| Typecast(ft, e) -> "(" ^ fulltype_to_string ft ^ ") " ^ expr_to_string e 
| Question(e1, e2, e3) ->  expr_to_string e1 ^ " ? " ^ expr_to_string e2 ^ " : " ^ expr_to_string e3
| New(ft, None) -> "new " ^ fulltype_to_string ft
| New(Type(t, n), Some(e)) -> "new " ^ fulltype_to_string (Type(t, n)) ^ "[" ^ expr_to_string e ^ "]"
| Delete(e) -> "delete " ^ expr_to_string e

let ft_equal t1 t2 = 
  match t2 with
  | Type(Basic, -1) -> 
    (match t1 with
    | Type(Void, _) | Type(Label, _) | NULL -> false 
    | _ -> true)
  | Type(Basic, 0) ->
    (match t1 with
    | Type(Void, _) | Type(Label, _) | NULL -> false 
    | Type(_, 0) -> true
    | _ -> false)
  | Type(Basic, 1) -> 
    (match t1 with
    | Type(Void, _) | Type(Label, _) | NULL -> false 
    | Type(_, 0) -> false
    | _ -> true)
  | Type(BV, _) -> 
    (match t1 with
    | Type(Label, _) | NULL -> false 
    | _ -> true)
  | Type(Num, 0) -> 
      (match t1 with
      | Type(Int, 0) | Type(Double, 0) -> true
      | _ -> false)
  | _ -> t1 = t2

