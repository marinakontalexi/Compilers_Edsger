/* header */
%{
    open Printf
    open Ast

    let rec expr_to_list expression =
     match expression with 
     | Bin_operation(e1, COMMA, e2) -> (expr_to_list e1) @ (expr_to_list e2)
     | _ -> [expression]    
     let line_number = ref 0
%}

/* declarations */
%token <Ast.program> T_include
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
program: 
     | directives_list declaration_list EOF {syntaxTree := $1 @ (List.rev $2); raise (End_of_parser (!syntaxTree))}
;

directives_list:  { [] }
       | directives_list directive { $1 @ $2 }
;

directive: T_include { $1 }

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

variable_declaration: fulltype declarator_list SEMICOLON { Var_declaration($1, List.rev $2) }
                    | error declarator_list SEMICOLON { raise Parse_error; print_endline("!!Syntax error: Wrong type at line " ^ (string_of_int !line_number)); Var_declaration(Type(Void, 0), [])}

;

declarator_list: declarator { [$1] }
               | declarator_list COMMA declarator { $3::$1 }
;

fulltype: basic_type pointer { Type($1, $2) }
;

basic_type: INT { Int }
          | CHAR { Char }
          | BOOL { Bool }
          | DOUBLE { Double }
;

pointer: /* empty */ { 0 }
       | pointer TIMES %prec POINT { $1 + 1 }
;

declarator: ID table { Declarator(Id($1), $2) }
;

table: /* empty */ { None }
     | L_BRACK constant_expression R_BRACK { Some($2) }
;

function_declaration: fulltype ID L_PAREN parameter_list R_PAREN SEMICOLON { Fun_declaration($1, Id($2), List.rev $4) }
                    | VOID ID L_PAREN parameter_list R_PAREN SEMICOLON { Fun_declaration(Type(Void, 0), Id($2), List.rev $4) }
;

function_definition: fulltype ID L_PAREN parameter_list R_PAREN  
                        L_BRACE declaration_list_empty statement_list R_BRACE { Fun_definition($1, Id($2), List.rev $4, List.rev $7, List.rev $8) }
                   | VOID ID L_PAREN parameter_list R_PAREN  
                        L_BRACE declaration_list_empty statement_list R_BRACE { Fun_definition(Type(Void, 0), Id($2), List.rev $4, List.rev $7, List.rev $8) }
;


parameter_list: /* empty */ { [] }
              | parameter { [$1] }
              | parameter_list COMMA parameter { $3::$1 }
;

parameter: BYREF fulltype ID { Param(Byref, $2, Id($3)) }
         | fulltype ID { Param(Byvalue, $1, Id($2)) }
;


declaration_list_empty: /* empty */ { [] }
                       | declaration_list_empty declaration { $2::$1 }
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
          | ID L_PAREN R_PAREN { Fun_call(Id($1), [])}
          | ID L_PAREN expression R_PAREN { Fun_call(Id($1), (expr_to_list $3)) }
          | expression L_BRACK expression R_BRACK { Table_call($1, $3) } 
          | unary_expression { $1 }
          | binary_expression { $1 }
          | unary_assignment { $1 }
          | binary_assignment { $1 }
          | L_PAREN fulltype R_PAREN expression %prec L_TYPE { Typecast($2, $4) }
          | expression QUE expression DDOT expression { Question($1, $3, $5) }
          | NEW fulltype { New($2, None) }
          | NEW fulltype L_BRACK expression R_BRACK { New($2, Some($4)) }
          | DELETE expression { Delete($2) }
;

// brack_expr: /* empty */ { None }
//           | L_BRACK expression R_BRACK { Some($2) }
// ;

// expression_list: expression { [$1] }
//                | expression_list COMMA expression { $3::$1 }
// ;

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
                 | expression LEQ expression { Bin_operation($1, LEQ, $3)}
                 | expression GEQ expression { Bin_operation($1, GEQ, $3)}
                 | expression EQ expression { Bin_operation($1, EQ, $3)}
                 | expression NEQ expression { Bin_operation($1, NEQ, $3) }
                 | expression LOGICAL_AND expression { Bin_operation($1, LOGICAL_AND, $3) }
                 | expression LOGICAL_OR expression { Bin_operation($1, LOGICAL_OR, $3) }
                 | expression COMMA expression { Bin_operation($1, COMMA, $3)}
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