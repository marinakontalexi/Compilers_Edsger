(* header section *)
{ 
    open Printf
    open Char
    open Parser

    (* Exception *)
    let lexical_error_found = ref false
    let lexical_errors = ref []
    exception Lexical_Error of string

    let line_number = ref 1
    let keyword_table = Hashtbl.create 20
    let _ = 
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        ["bool", BOOL;
        "break", BREAK;
        "byref", BYREF;
        "char", CHAR;
        "continue", CONTINUE;
        "delete", DELETE;
        "double", DOUBLE;
        "else", ELSE;
        "false", FALSE;
        "for", FOR;
        "if", IF;
        "int", INT;
        "new", NEW;
        "NULL", NULL;
        "return", RETURN;
        "true", TRUE;
        "void", VOID]

    let operatoreq_table = Hashtbl.create 10
    let _ = 
        List.iter (fun (op, tok) -> Hashtbl.add operatoreq_table op tok)
        ["==", EQ;
        "!=", NEQ;
        "<=", LEQ;
        ">=", GEQ;
        "+=", PLUSEQ;
        "-=", MINUSEQ;
        "*=", TIMESEQ;
        "/=", DIVEQ;
        "%=", MODEQ;
        "++", INCR;
        "--", DECR;
        "&&", LOGICAL_AND;
        "||", LOGICAL_OR]

    let operator_table = Hashtbl.create 22
        let _ = 
            List.iter (fun (op, tok) -> Hashtbl.add operator_table op tok)
            ['=', ASSIGN;
            '<', LESS;
            '>', MORE;
            '+', PLUS;
            '-', MINUS;
            '*', TIMES;
            '/', DIV;
            '%', MOD;
            '&', AND;
            '!', EXC;
            '?', QUE;
            ':', DDOT;
            ',', COMMA;
            ';', SEMICOLON;
            '(', L_PAREN;
            ')', R_PAREN;
            '[', L_BRACK;
            ']', R_BRACK;
            '{', L_BRACE;
            '}', R_BRACE]

    (* let t_eof = 0
    let t_id = 1018
    let t_const_i = 1019
    let t_const_f = 1020
    let t_const_c = 1021
    let t_const_s = 1022 *)
}

(* definitions section *)

let L = ['a'-'z' 'A'-'Z']
let D = ['0'-'9']
let W = [' ' '\t' '\r' '\n']
let HEX = ['0'-'9' 'A'-'F']
let common_c = [^ '\'' '\"' '\\' '\n']
let escape = '\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | 'x' HEX HEX)
(* let stringescape = '\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | 'x' HEX HEX) *)

(* rules section *)
rule eds_lex = parse
    | L (L | D | "_")* as id
        { try
            Hashtbl.find keyword_table id
          with Not_found ->
              ID id }
    | D+ as const_int { CONST_I (int_of_string const_int) }
    | D+ "." D+ (("e" | "E") ("+" | "-")?  D+)? as const_f { CONST_F (float_of_string const_f) }
    (* | ''' ((common_c | escape) as c) '''  { CONST_C c } *)
    | '"' (common_c | escape)+ '"' as const_s { CONST_S const_s }
    | ['=' '!' '+' '-' '*' '/' '%' '>' '<']'=' | "++" | "--" | "&&" | "||" 
        as op2 { Hashtbl.find operatoreq_table op2 }
    | ['=' '>' '<' '+' '-' '*' '/' '%' '&' '!' '?' ':' ',' '(' ')' '[' ']' '{' '}' ';'] 
        as op { Hashtbl.find operator_table op }
    | ['\n'] { incr line_number; eds_lex lexbuf }
    | W+ 
    | "//"[^'\n']+ { eds_lex lexbuf }
    | "/*" { comment lexbuf }
    (* | "/*" ([^'*']+ | '*'+ [^'*' '/'])* '*'+ "/" { eds_lex lexbuf } *)
    | eof { if !lexical_error_found then               
                let f (c,l) =
                    "\nUnrecognized character:" ^ (String.make 1 c) ^ " at line: " ^ (string_of_int l)
                in
                    raise (Lexical_Error (List.fold_left (^) " " (List.map f (List.rev !lexical_errors))))
            else
                EOF 
          }
    | _ as c
        { 
            lexical_error_found := true;
            lexical_errors := (c, !line_number) :: !lexical_errors;
            CONST_C c
        
        }
    and comment = parse 
    | "*/" { eds_lex lexbuf }
    | '\n' { incr line_number; comment lexbuf }
    | _ { comment lexbuf }

(* trailer section *)
{
    (* let rec parse lexbuf = 
        let token = eds_lex lexbuf in
        (* printf "token: %d String: %s\n" token s;  *)
        parse lexbuf
    
    let main () =
        let cin =
        if Array.length Sys.argv > 1
        then open_in Sys.argv.(1)
        else stdin
        in
        let lexbuf = Lexing.from_channel cin in
        try parse lexbuf
        with End_of_file -> ()
        let _ = Printexc.print main () *)
}
