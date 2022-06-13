/* header */
%{
    open Printf
    type program = Program of declaration list
    type declaration = Var_declaration of fulltype * declarator list
                     | Fun_declaration of fulltype * id * parameter list
                     | Fun_definition of fulltype * id * parameter list * declaration list * stmt list
    type fulltype = Type of basic_type * int (*pointer list if we use star*)
    type basic_type = Int | Char | Bool | Double | Void        (* basic+result type *)
   (* type pointer = Star *)
    type declarator = Declarator of id * constant_expr option
    type parameter = Param of call * fulltype * id
    type call = None | Byref
    type stmt = Empty_stmt
              | Expression of expr
              | Stmt_block of stmt list
              | If of expr * stmt * stmt option
              | For of id option * expr option * expr option * expr option * stmt
              | Continue of id option
              | Break of id option
              | Return of expr option
    type expr = Id of id
              | True | False | NULL 
              | INT of int
              | CHAR of char
              | DOUBLE of float
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
    type id = Id of string
    type un_op = AND | POINT | POS | NEG | EXC
    type bin_op = TIMES | DIV | MOD | PLUS | MINUS 
                | LESS | MORE | LEQ | GEQ | EQ | NEQ
                | LOGICAL_AND | LOGICAL_OR | COMMA
    type un_assign = INCR | DECR
    type bin_assign = ASSIGN | TIMESEQ | DIVEQ | MODEQ | PLUSEQ | MINUSEQ

%}

/* declarations */
%token BOOL BREAK BYREF
%token CHAR CONTINUE
%token <char> CONST_C
%token <float> CONST_F
%token <int> CONST_I
%token <string> CONST_S
%token DELETE DOUBLE
%token ELSE FALSE FOR
%token <string> ID
%token IF INT
%token NEW NULL
%token RETURN TRUE VOID
%token ASSIGN
%token MORE LESS
%token PLUS MINUS TIMES DIV MOD 
%token AND EXC QUE
%token DDOT COMMA SEMICOLON
%token L_PAREN R_PAREN L_BRACK R_BRACK L_BRACE R_BRACE 
%token EQ NEQ LEQ GEQ
%token PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ 
%token INCR DECR 
%token LOGICAL_AND LOGICAL_OR
%token EOF

%left COMMA
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ
%nonassoc QUE DDOT // ?:
%left LOGICAL_OR
%left LOGICAL_AND
%nonassoc EQ NEQ LESS MORE LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right L_TYPE // typecast
%left R_TYPE
%nonassoc L_INCR L_DECR // den mporo na grapsw ++a++b
%nonassoc NEW DELETE
%right AND POINT POS NEG EXC // & * + - !
%nonassoc INCR DECR // den mporo na grapsw a++b++
%right L_PAREN L_BRACK 
%left R_PAREN R_BRACK

%start program
%type<unit> program

%%

/* grammar rules */
program: declaration_list {Program(List.rev $1)}
;

declaration_list: declaration { [$1] }
       | declaration_list declaration { $2::$1 }
;

// program: declaration { Program([$1]) }
//        | program declaration { append($2,$1) } /* need append function */
// ;

declaration: variable_declaration { $1 }
           | function_declaration { $1 }
           | function_definition { $1 }
;

variable_declaration: type declarator_list SEMICOLON { Var_declaration($1, List.rev $2) }
;

declarator_list: declarator { [$1] }
               | declarator_list COMMA declarator { $3::$1 }
;

type: basic_type pointer { Type($1, $2) }
;

basic_type: INT { Int }
          | CHAR { Char }
          | BOOL { Bool }
          | DOUBLE { Double }
;

pointer: /* empty */ { 0 }
       | pointer TIMES { $1 + 1 }
;

declarator: ID table { Declarator(Id($1), $2) }
;

table: /* empty */ { None }
     | L_BRACK constant_expression R_BRACK { $2 }
;

function_declaration: result_type ID L_PAREN parameter_list R_PAREN SEMICOLON { Fun_declaration($1, Id($2), List.rev $4) }
;

result_type: type { $1 }
           | VOID { Type(Void, 0) }
;

parameter_list: parameter { [$1] }
              | parameter_list COMMA parameter { $3::$1 }
;

parameter: BYREF type ID { Param(Byref, $2, Id($3)) }
         | type ID { Param(None, Id($2)) }
;

function_definition: result_type ID L_PAREN parameter_list R_PAREN   
                        L_BRACE declaration_list statement_list R_BRACE { Fun_definition($1, Id($2), $4, List.rev $7, List.rev $8) }
;

declaration_list: /* empty */ { [] }
                | declaration_list declaration { $2::$1 }
;

statement_list: /* empty */ { [] }
              | statement_list statement { $2::$1 }
;

statement: SEMICOLON { Empty_stmt }
         | expression SEMICOLON { Expression($1) }
         | L_BRACE statement_list R_BRACE { Stmt_block(List.rev $2) }   
         | IF L_PAREN expression R_PAREN statement else_statement { If($3, $5, $6) }
         | ID DDOT FOR L_PAREN empty_expression SEMICOLON empty_expression SEMICOLON empty_expression R_PAREN statement { For(Some(Id($1)), $5, $7, $9, $11) }
         | FOR L_PAREN empty_expression SEMICOLON empty_expression SEMICOLON empty_expression R_PAREN statement { For(None, $3, $5, $7, $9) }
         | CONTINUE empty_id SEMICOLON { Continue($2) }
         | BREAK empty_id SEMICOLON { Break($2) }
         | RETURN empty_expression SEMICOLON { Return($2) }
;

else_statement: /* empty */ { None }
              | ELSE statement { Some($2) }
;

empty_expression: /* empty */ { None }
              | expression { Some($1) }
;

empty_id: /* empty */ { None }
        | ID { Some(Id($1)) }
;

expression: ID { Id($1) }             
          | L_PAREN expression R_PAREN { $2 }
          | TRUE { True }
          | FALSE { False }
          | NULL { NULL }
          | CONST_I { INT($1) }
          | CONST_C { CHAR($1) }
          | CONST_F { FLOAT($1) }
          | CONST_S { STRING($1) }
          | ID L_PAREN empty_expr_list R_PAREN { Fun_call(Id($1), $3) }
          | expression L_BRACK expression R_BRACK { Table_call($1, $3) }   /* mono gia *id[expr]?? */
          | unary_expression { $1 }
          | binary_expression { $1 }
          | unary_assignment { $1 }
          | binary_assignment { $1 }
          | L_PAREN type R_PAREN expression %prec L_TYPE %prec R_TYPE{ Typecast($2, $4) }
          | expression QUE expression DDOT expression { Question($1, $3, $5) }
          | NEW type brack_expr { New($2, $3) }
          | DELETE expression { Delete($2) }
;

brack_expr: /* empty */ { None }
          | L_BRACK expression R_BRACK { Some($2) }
;

empty_expr_list: /* empty */ { [] }
               | expression_list { List.rev $1 }
;

expression_list: expression { [$1] }
               | expression_list COMMA expression { $3::$1 }
;

constant_expression: expression { Const_expr($1) }
;

unary_expression: AND expression { Un_operation(AND, $2) }
                | TIMES expression %prec POINT { Un_operation(POINT, $2) }
                | PLUS expression %prec POS { Un_operation(POS, $2) }
                | MINUS expression %prec NEG { Un_operation(NEG, $2) }
                | EXC expression { Un_operation(EXC, $2) }
;

binary_expression: expression TIMES expression { Bin_operation($1, TIMES, $3) } 
                 | expression DIV expression { Bin_operation($1, DIV, $3) }
                 | expression MOD expression { Bin_operation($1, MOD, $3) }
                 | expression PLUS expression { Bin_operation($1, PLUS, $3)}
                 | expression MINUS expression { Bin_operation($1, MINUS, $3) }
                 | expression LESS expression { Bin_operation($1, LESS, $3)}
                 | expression MORE expression { Bin_operation($1, MORE, $3)}
                 | expression LEQ expression { Bin_operation($1, LEQ, $2)}
                 | expression GEQ expression { Bin_operation($1, GEQ, $2)}
                 | expression EQ expression { Bin_operation($1, EQ, $2)}
                 | expression NEQ expression { Bin_operation($1, NEQ, $2) }
                 | expression LOGICAL_AND expression { Bin_operation($1, LOGICAL_AND, $2) }
                 | expression LOGICAL_OR expression { Bin_operation($1, LOGICAL_OR, $2) }
                 | expression COMMA expression { Bin_operation($1, COMMA, $2)}
;

unary_assignment: expression INCR { Un_assignment_right($1, INCR) }
                | expression DECR { Un_assignment_right($1, DECR) }
                | INCR expression %prec L_INCR { Un_assignment_left(INCR, $2) }
                | DECR expression %prec L_DECR { Un_assignment_left(DECR, $2) }
;

binary_assignment: expression ASSIGN expression { Bin_assignment($1, ASSIGN, $3) }
                 | expression TIMESEQ expression { Bin_assignment($1, TIMESEQ, $3) }
                 | expression DIVEQ expression { Bin_assignment($1, DIVEQ, $3) }
                 | expression MODEQ expression { Bin_assignment($1, MODEQ, $3) }
                 | expression PLUSEQ expression { Bin_assignment($1, PLUSEQ, $3) }
                 | expression MINUSEQ expression { Bin_assignment($1, MINUSEQ, $3) }
;

%%

(* trailer *)