open Lexer
open Parser
open Semantic
open Symbol
open Print_fun
open Lexing
open Printf

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
    let lexbuf = Lexing.from_channel cin in
    try
        while true do
          Parser.program Lexer.eds_lex lexbuf;
        done
    with
    | Parsing.Parse_error -> print_endline("Syntax error at line " ^ string_of_int !line_number)
    | Lexer.Lexical_Error p -> print_endline p
    | End_of_file -> 
      (print_endline "Syntax analysis OK\n";
      try 
        List.map Semantic.semantic (List.map Symbol.decl_to_sem !Ast.syntaxTree);
        Semantic.print_semantic_error ()
      with  
      | Semantic.Semantic_Error p -> print_endline p
      | End_of_semantic -> print_endline "Semantic analysis OK\n")


let _ = main(); Print_fun.print !Ast.syntaxTree
(* let _ = Printexc.print main () *)