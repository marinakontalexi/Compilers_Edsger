(* header section *)
{ 
    open Lexing
    open Printf
    open Char
    open Parser

    (* Exception *)
    let lexical_error_found = ref false
    let lexical_errors = ref []
    exception Lexical_Error of string
    exception End_of_parser of Ast.program

    module StringSet = Set.Make(String)
    type globalSet = StringSet.t ref
    let set : globalSet = ref StringSet.empty

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

    let escape_table = Hashtbl.create 10
        let _ = 
            List.iter (fun (op, tok) -> Hashtbl.add escape_table op tok)
            ["\'\\n\'", CONST_C '\n';
            "\'\\t\'", CONST_C '\t';
            "\'\\r\'", CONST_C '\r';
            "\'\\\\'", CONST_C '\\';
            "\'\\\'\'", CONST_C '\'';
            "\'\\\"\'", CONST_C '\"';
            "\'\\0\'", CONST_C (Char.chr 0)]
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
let escape = ['n' 't' 'r' '0' '\\' '\'' '\"']
let escape_str = '\\'

let name = '\"' (([^'"'' ''\t''\n'])+ as filename) '\"' 
let include_directive = "#include" W* name

(* rules section *)
rule eds_lex = parse
    | include_directive {
        let deafult_lib_header = "/mnt/c/Users/geopa/OneDrive/Documents/sxoli/ROH_L/Compilers/Compilers_Edsger/lexer/lib/lib-headers" in
        let fn = ref filename in
        let cd = Sys.getcwd() in
        let starts_with_dot str =
            let prefix = "." in
            let prefix_length = String.length prefix in
            let str_length = String.length str in
            str_length >= prefix_length && String.sub str 0 prefix_length = prefix
        in
        let cnf = 
            if starts_with_dot filename then (
                (* let dir = Filename.dirname filename in
                let relative_path = Filename.concat dir filename in
                let full_path = Filename.concat (Sys.getcwd ()) relative_path in
                Printf.printf "path: %s\n" full_path; *)
                (* "\027[1;%s (File '%s' - Line %d, Column %d): \027[0m%s\n" *)
                let msg = "We are sorry we do not accept relatives paths :)\nPlease give the full path of the file you want to include a file that is not in default libraries!" in
                Printf.eprintf "\027[31m%s\027[0m \n" msg;
                exit 1
            )
            else filename in
        Printf.printf "cnf: %s\n" cnf;
        if (not (Sys.file_exists cnf)) then(
            let lib_cnf = Printf.sprintf "%s/%s" deafult_lib_header filename in
            if (not (Sys.file_exists lib_cnf)) 
            then(
                Printf.printf "File '%s' does not exists\n" lib_cnf;
                exit 1
            )
            else fn := lib_cnf
        )
        else(
            fn := cnf
        );
        
        let find name = 
        try 
            Some (StringSet.find name !set) 
        with
            | Not_found -> None
        in 
        let f = find filename in
        match f with 
        | None ->
            set := StringSet.add filename !set;
            let lb = Stdlib.open_in !fn |> Lexing.from_channel in 
            try(
                while true do
                Parser.program eds_lex lb;
                done;
                T_include [])
            with 
            | Ast.End_of_parser tree  -> T_include tree
            | _ -> Printf.printf "File '%s' is not correct\n" filename; exit 1
        | _ -> T_include []

    }

    | L (L | D | "_")* as id
        { try
            Hashtbl.find keyword_table id
          with Not_found ->
              ID id }
    | D+ as const_int { CONST_I (int_of_string const_int) }
    | D+ "." D+ (("e" | "E") ("+" | "-")?  D+)? as const_f { CONST_F (float_of_string const_f) }
    | '\'' (common_c as c) '\'' as s { CONST_C c }
    | "\'\\" (escape as c) "\'" as e {
        try
        Hashtbl.find escape_table e 
        with Not_found ->  (lexical_error_found := true;
                            lexical_errors := (c, !line_number) :: !lexical_errors;
                            eds_lex lexbuf)
    }
    | "\'\\" 'x' (HEX as h1) (HEX as h2) '\'' { let i = int_of_string ("0x" ^ (String. make 1 h1) ^ (String. make 1 h2)) in CONST_C (Char.chr i)}
    | '\"' (common_c | escape_str)* '\"' as const_s { CONST_S const_s }
    | ['=' '!' '+' '-' '*' '/' '%' '>' '<']'=' | "++" | "--" | "&&" | "||" 
        as op2 { Hashtbl.find operatoreq_table op2 }
    | ['=' '>' '<' '+' '-' '*' '/' '%' '&' '!' '?' ':' ',' '(' ')' '[' ']' '{' '}' ';'] 
        as op { Hashtbl.find operator_table op }
    | ['\n'] {incr line_number; print_endline "new_line";Lexing.new_line lexbuf; eds_lex lexbuf }
    | W+ 
    | "//"[^'\n']+ { eds_lex lexbuf }
    | "/*" { comment lexbuf }
    (* | "/*" ([^'*']+ | '*'+ [^'*' '/'])* '*'+ "/" { eds_lex lexbuf } *)
    | eof { if !lexical_error_found then               
                let f (c,l) =
                    "Unrecognized character:" ^ (String.make 1 c) ^ " at line: " ^ (string_of_int l) ^ "\n"
                in
                    raise (Lexical_Error (List.fold_left (^) " " (List.map f (List.rev !lexical_errors))))
            else
                EOF
          }
    | _ as c
        { 
            lexical_error_found := true;
            lexical_errors := (c, !line_number) :: !lexical_errors;
            (* CONST_C c *)
            eds_lex lexbuf
        }
    and comment = parse 
    | "*/" { eds_lex lexbuf }
    | '\n' { incr line_number; Lexing.new_line lexbuf; Lexing.flush_input lexbuf;comment lexbuf }
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
    (* print_endline "hey from lexer";
    print_endline (string_of_int !line_number) *)
}