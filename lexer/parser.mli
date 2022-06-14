type token =
  | BOOL
  | BREAK
  | BYREF
  | CHAR
  | CONTINUE
  | CONST_C of (char)
  | CONST_F of (float)
  | CONST_I of (int)
  | CONST_S of (string)
  | DELETE
  | DOUBLE
  | ELSE
  | FALSE
  | FOR
  | ID of (string)
  | IF
  | INT
  | NEW
  | NULL
  | RETURN
  | TRUE
  | VOID
  | ASSIGN
  | MORE
  | LESS
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | AND
  | EXC
  | QUE
  | DDOT
  | COMMA
  | SEMICOLON
  | L_PAREN
  | R_PAREN
  | L_BRACK
  | R_BRACK
  | L_BRACE
  | R_BRACE
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | PLUSEQ
  | MINUSEQ
  | TIMESEQ
  | DIVEQ
  | MODEQ
  | INCR
  | DECR
  | LOGICAL_AND
  | LOGICAL_OR
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
