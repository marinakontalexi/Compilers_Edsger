/* header */
%{
    open Printf
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
/* ?: proseteristikotita */
%left LOGICAL_OR
%left LOGICAL_AND
%nonassoc EQ NEQ LESS MORE LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
/* TBRevisited: */
%nonassoc L_PAREN R_PAREN
%nonassoc INCR DECR
%nonassoc NEW DELETE
%nonassoc AND EXC // syn: + - * gia apodeiktodotisi kai prosima
%nonassoc L_BRACK R_BRACK

%start input
%type <unit> input

%%

/* grammar rules */
input: /* empty */ { }
    | input line { }
;

line: SEMICOLON { }
| exp SEMICOLON { printf "\t%d\n" $1; flush stdout }
;

exp: CONST_I { $1 }
| exp PLUS exp { $1 + $3 }
| exp MINUS exp { $1 - $3 }
| exp TIMES exp { $1 * $3 }
| exp DIV exp { $1 / $3 }
| exp MOD exp { $1 mod $3 }
;

%%

(* trailer *)