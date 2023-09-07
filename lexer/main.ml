open Lexer
open Parser
open Semantic
open Symbol
open Print_fun
open Lexing
open Printf
open Llvm
open Llvm_analysis
open Codegen
(* open Optimizer *)

(* let main () =
  try
  let lexbuf = Lexing.from_channel stdin in
  while true do
  Parser.program Lexer.eds_lex lexbuf
  done
  with End_of_file -> exit 0
let _ = Printexc.print main () *)

let opt_flag = ref true

let filename = ref ""
let speclist =  []
let usage_msg = "\027[93mUsage:\027[0m ./edsger [options] <file>.eds \n\027[93mOptions:\027[0m"

let main () =
  let cin = 
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else "stdin"
  in
  let lexbuf = Stdlib.open_in cin |> Lexing.from_channel in
    try
        while true do
          Parser.program Lexer.eds_lex lexbuf;
        done
    with
    | Parsing.Parse_error -> (
      let pos = lexbuf.Lexing.lex_curr_p in
      let msg = "Syntax Error" in 
      let txt = "31mError" in
      let line_pos = (pos.pos_cnum - pos.pos_bol + 1) in
                  Printf.eprintf "\027[1;%s (File '%s' - Line %d, Column %d): \027[0m%s\n"
                                txt pos.pos_fname pos.pos_lnum line_pos msg
      )
    | Lexer.Lexical_Error p -> (print_endline p;)
    | Ast.End_of_parser tree-> 
      print_endline "Syntax analysis OK\n";
      try 
        List.map Semantic.semantic (List.map Symbol.decl_to_sem !Ast.syntaxTree);
        Semantic.print_semantic_error ()
      with  
      | Semantic.Semantic_Error p -> print_endline p
      | End_of_semantic -> 
        print_endline "Semantic analysis OK\n";
        let llm = List.map Codegen.codegen_decl !Ast.syntaxTree in
        (* if (!opt_flag) then Optimizer.optimize Codegen.the_module; *)
        print_module "a.ll" Codegen.the_module



let _ = main() (*; Print_fun.print !Ast.syntaxTree*)
(* let _ = Printexc.print main () *)