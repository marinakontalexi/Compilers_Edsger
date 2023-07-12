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
    Lexing.lexeme_start lexbuf;
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Sys.argv.(1) };
    Lexing.new_line lexbuf;
    try
        while true do
          ignore (Parser.program Lexer.eds_lex lexbuf);
        done
    with
    | Parsing.Parse_error -> (
      let error_msg = Printexc.to_string (Parsing.Parse_error) in 
      let pos = lexbuf.Lexing.lex_curr_p in
      (* match pos with 
      | None -> ()
      | Some p ->  *)
      let line_pos = (pos.pos_cnum - pos.pos_bol + 1) in
      (* let line_pos = (pos.pos_lnum ) in *)
      print_endline("1.Syntax error at line " ^ string_of_int line_pos ^ "\n\tERROR: " ^ error_msg);
      print_endline("2.Syntax error at line " ^ string_of_int !line_number ^ "\n\tERROR: " ^ error_msg);
      let error_position = lexbuf.lex_start_p in
      let line_number = error_position.pos_lnum in
      Printf.printf "3.Syntax error on line %d\n" line_number;
      let error_position = lexbuf.lex_curr_p in
      let line_number = error_position.pos_lnum in
      let line_start = error_position.pos_bol in
      let current_position = error_position.pos_cnum - line_start in
      Printf.printf "4.Syntax error at line %d, position %d\n" line_number current_position;
      let error_position = lexbuf.lex_start_p in
      let line_number = error_position.pos_lnum in
      Printf.printf "5.Syntax error on line %d\n" line_number)


    | Lexer.Lexical_Error p -> print_endline p;
    | Ast.End_of_parser tree-> 
      print_endline "Syntax analysis OK\n";
      try 
        List.map Semantic.semantic (List.map Symbol.decl_to_sem !Ast.syntaxTree);
        Semantic.print_semantic_error ()
      with  
      | Semantic.Semantic_Error p -> print_endline p
      | End_of_semantic -> 
        print_endline "Semantic analysis OK\n";
        (* let create_basic_block () =
          let function_type = function_type (i32_type context) [||] in
          let function_value = define_function "program" function_type the_module in
          let entry_block = append_block context "entry" function_value in
          position_at_end entry_block builder;
          entry_block
        in
        let current_block = create_basic_block () in
        let _ = insertion_block builder in *)
        let llm = List.map Codegen.codegen_decl !Ast.syntaxTree in
        print_module "a.ll" Codegen.the_module



let _ = main() (*; Print_fun.print !Ast.syntaxTree*)
(* let _ = Printexc.print main () *)