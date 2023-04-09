open Lexer
open Parser

(* let main () =
  try
  let lexbuf = Lexing.from_channel stdin in
  while true do
  Parser.program Lexer.eds_lex lexbuf
  done
  with End_of_file -> exit 0
let _ = Printexc.print main () *)


let main () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
    try
      let lexbuf = Lexing.from_channel cin in
        while true do
          Parser.program Lexer.eds_lex lexbuf
        done
    with
    | Parsing.Parse_error -> print_endline("Syntax error at line: " ^ (string_of_int !line_number))
    | Lexer.Lexical_Error p -> print_endline(p)
    | End_of_file -> Ast.print !Ast.syntaxTree (* exit 0 *)
        (* let _ = Printexc.print main () *)
    (* sematic ast;
       print_sem_error *)
      
let _ = main(); Ast.print !Ast.syntaxTree