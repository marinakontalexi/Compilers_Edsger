all: main clean

LLVM_PATH=~/.opam/default/lib/llvm/
LLVM_FLAGS=-I $(LLVM_PATH) -c

main: ast.cmo semantic.cmo lexer.cmo parser.cmo print_fun.cmo  address_records.cmo codegen.cmo main.cmo 
	ocamlc -o main lexer.cmo ast.cmo parser.cmo symbol.cmo semantic.cmo print_fun.cmo  address_records.cmo codegen.cmo main.cmo 

main.cmi main.cmo: main.ml 
	# ocamlc -c main.ml
	ocamlopt $(LLVM_FLAGS) main.ml

address_records.cmo: address_records.ml
	ocamlopt $(LLVM_FLAGS) address_records.ml

codegen.cmo: codegen.ml ast.cmo address_records.cmo
	ocamlopt $(LLVM_FLAGS) codegen.ml

semantic.cmo: semantic.ml ast.cmo symbol.cmo lexer.cmi
	ocamlc -c semantic.ml

parser.cmo: parser.ml lexer.cmi 
	ocamlc -c parser.ml

lexer.cmi lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.exe parser.ml parser.mli: parser.mly 
	ocamlyacc -v parser.mly 

symbol.cmo: symbol.ml ast.cmo
	ocamlc -c symbol.ml

print_fun.cmo: print_fun.ml ast.cmo
	ocamlc -c print_fun.ml

ast.cmo: ast.ml
	ocamlc -c ast.ml

clean: 
	rm parser.output parser.ml parser.cmo parser.cmi parser.mli ast.cmi ast.cmo main.cmi main.cmo lexer.cmi lexer.cmo lexer.ml symbol.cmi symbol.cmo semantic.cmi semantic.cmo print_fun.cmi print_fun.cmo