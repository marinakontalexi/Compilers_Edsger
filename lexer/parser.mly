/* header */
%{
    open Printf
    type program = Program of declaration list
    type declaration = Var_declaration of fulltype * declarator list
                     | Fun_declaration of fulltype * id * parameter list
                     | Fun_definition of fulltype * id * parameter list * declaration list * stmt list
    type fulltype = Type of basic_type * pointer list
    type basic_type = Int | Char | Bool | Double | Void        (* basic+result type *)
    type pointer = Star
    type declarator = Declarator of id * constant_expr option
    type parameter = Param of call * fulltype * id
    type call = None | Byref
    type stmt = Empty
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
program: declaration { }
       | program declaration { }
;

declaration: variable_declaration { }
           | function_declaration { }
           | function_definition { }
;

variable_declaration: type declarator_list SEMICOLON { }
;

declarator_list: declarator { }
               | declarator_list COMMA declarator { }
;

type: basic_type pointer { }
;

basic_type: INT { }
          | CHAR { }
          | BOOL { }
          | DOUBLE { }
;

pointer: /* empty */ { }
       | pointer TIMES { }
;

declarator: ID table { }
;

table: /* empty */ { }
     | L_BRACK constant_expression R_BRACK { }
;

function_declaration: result_type ID L_PAREN parameter_list R_PAREN SEMICOLON { }
;

result_type: type { }
           | VOID { }
;

parameter_list: parameter { }
              | parameter_list COMMA parameter { }
;

parameter: BYREF type ID { }
         | type ID { }
;

function_definition: result_type ID L_PAREN parameter_list R_PAREN 
                        L_BRACE declaration_list statement_list R_BRACE { }
;

declaration_list: /* empty */ { }
                | declaration_list declaration { }
;

statement_list: /* empty */ { }
              | statement_list statement { }
;

statement: SEMICOLON { }
         | expression SEMICOLON { }
         | L_BRACE statement_list R_BRACE { }   
         | IF L_PAREN expression R_PAREN statement else_statement { }
         | ID DDOT for_statement { }
         | for_statement { }
         | CONTINUE empty_id SEMICOLON { }
         | BREAK empty_id SEMICOLON { }
         | RETURN empty_expression SEMICOLON { }
;

else_statement: /* empty */ { }
              | ELSE statement { }
;

for_statement: FOR L_PAREN empty_expression SEMICOLON empty_expression SEMICOLON empty_expression R_PAREN statement { }
;

empty_expression: /* empty */ { }
              | expression { }
;

empty_id: /* empty */ { }
        | ID { }
;

expression: ID id_expr { }             /* <I> , function call */
          | L_PAREN expression R_PAREN { }
          | TRUE { }
          | FALSE { }
          | NULL { }
          | CONST_I { }
          | CONST_C { }
          | CONST_F { }
          | CONST_S { }
          | expression L_BRACK expression R_BRACK { }   /* mono gia *id[expr]?? */
          | unary_expression { }
          | binary_expression { }
          | unary_assignment { }
          | binary_assignment { }
          | L_PAREN type R_PAREN expression %prec L_TYPE %prec R_TYPE{ }
          | expression QUE expression DDOT expression { }
          | NEW type brack_expr { }
          | DELETE expression { }
;

id_expr: /* empty */ { }
       | L_PAREN empty_expr_list R_PAREN { }
;

brack_expr: /* empty */ { }
          | L_BRACK expression R_BRACK { }
;

empty_expr_list: /* empty */ { }
               | expression_list { }
;

expression_list: expression { }
               | expression_list COMMA expression { }
;

constant_expression: expression { }
;

unary_expression: AND expression { }
                | TIMES expression %prec POINT { }
                | PLUS expression %prec POS { }
                | MINUS expression %prec NEG { }
                | EXC expression { }
;

binary_expression: expression TIMES expression { } 
                 | expression DIV expression { }
                 | expression MOD expression { }
                 | expression PLUS expression { }
                 | expression MINUS expression { }
                 | expression LESS expression { }
                 | expression MORE expression { }
                 | expression LEQ expression { }
                 | expression GEQ expression { }
                 | expression EQ expression { }
                 | expression NEQ expression { }
                 | expression LOGICAL_AND expression { }
                 | expression LOGICAL_OR expression { }
                 | expression COMMA expression { }
;

unary_assignment: expression INCR { }
                | expression DECR { }
                | INCR expression %prec L_INCR { }
                | DECR expression %prec L_DECR { }
;

binary_assignment: expression ASSIGN expression { }
                 | expression TIMESEQ expression { }
                 | expression DIVEQ expression { }
                 | expression MODEQ expression { }
                 | expression PLUSEQ expression { }
                 | expression MINUSEQ expression { }
;

%%

(* trailer *)