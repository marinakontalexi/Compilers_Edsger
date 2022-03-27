
(* header section *)
{ 
    open Printf
    open Char

    let line_number = ref 1
    let keyword_table = Hashtbl.create 20
    let _ = 
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        ["bool", 1001;
        "break", 1002;
        "byref", 1003;
        "char", 1004;
        "continue", 1005;
        "delete", 1006;
        "double", 1007;
        "else", 1008;
        "false", 1009;
        "for", 1010;
        "if", 1011;
        "int", 1012;
        "new", 1013;
        "NULL", 1014;
        "return", 1015;
        "true", 1016;
        "void", 1017]

    let operator_table = Hashtbl.create 10
    let _ = 
        List.iter (fun (kwd, tok) -> Hashtbl.add operator_table kwd tok)
        ["==", 2000;
        "<=", 2001;
        ">=", 2002;
        "+=", 2003;
        "-=", 2004;
        "*=", 2005;
        "/=", 2006;
        "%=", 2007]

    let t_eof = 0
    let t_id = 1018
    let t_const_i = 1019
    let t_const_f = 1020
    let t_const_c = 1021
    let t_const_s = 1022
}

(* definitions section *)

let L = ['a'-'z' 'A'-'Z']
let D = ['0'-'9']
let W = [' ' '\t' '\r' '\n']
let HEX = ['0'-'9' 'A'-'F']
let common_c = [^ ' ' '\'' '\"' '\\']
let escape = '\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | 'x' HEX HEX)

(* rules section *)

rule eds_lex = parse
    | L (L | D | "_")* as id
        { try
            (Hashtbl.find keyword_table id, id)
          with Not_found ->
             (t_id, id) }
    | D+ as const_int { (t_const_i, const_int) }
    | D+ "." D+ (("e" | "E") ("+" | "-")?  D+)? as const_f { (t_const_f, const_f) }
    | ''' (common_c | escape) ''' as const_c { (t_const_c, const_c) }
    | '"' (common_c | escape)+ '"' as const_s { (t_const_s, const_s) }
    | ['=' '+' '-' '*' '/' '%' '>' '<']'=' as op2 { (Hashtbl.find operator_table op2, op2) }
    | ['=' '>' '<' '+' '-' '*' '/' '%' '&' '!' '?' ':' ',' '(' ')' '[' ']' '{' '}'] as op { (code op, Char.escaped op) }
    | ['\n'] { incr line_number; eds_lex lexbuf}
    | W+ 
    | "//"[^'\n']+ 
    | "/*" ([^'*']+ | '*'+ [^'*' '/'])* '*'+ "/" { eds_lex lexbuf }
    | eof { raise End_of_file }
    | _ as c
        { printf "Unrecognized character: %c at line: %d\n" c !line_number;
          (-1, Char.escaped c)
        }

(* trailer section *)
{
    let rec parse lexbuf =
        let (token, s) = eds_lex lexbuf in
        printf "token: %d String: %s\n" token s; 
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
        let _ = Printexc.print main ()
}

