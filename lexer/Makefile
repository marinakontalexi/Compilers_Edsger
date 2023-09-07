LIBPATH=./lib


LLVM_PATH=~/.opam/4.14.0/lib/llvm/
LLVM_FLAGS=-I $(LLVM_PATH) -c
LLVM_FILES=shared/llvm.cmxa shared/llvm_analysis.cmxa


all: main library clean
# LLVM_PATH=~/.opam/default/lib/llvm/
# LLVM_VERS=llvm-14
# LLVM_FLAGS=-cc g++ -ccopt -L$(LLVM_PATH) -I +$(LLVM_VERS)
# LLVM_FILES=llvm.cmxa llvm_analysis.cmxa	

# with optimizer
# main: ast.cmx semantic.cmx parser.cmx print_fun.cmx address_records.cmx codegen.cmx optimizer.cmx main.cmx 
# 	ocamlopt -o edsger -I $(LLVM_PATH) $(LLVM_FILES) ast.cmx parser.cmx lexer.cmx   symbol.cmx semantic.cmx print_fun.cmx address_records.cmx codegen.cmx optimizer.cmx main.cmx 

# without optimizer
main: ast.cmx semantic.cmx parser.cmx print_fun.cmx address_records.cmx codegen.cmx main.cmx 
	ocamlopt -o edsger -I $(LLVM_PATH) $(LLVM_FILES) ast.cmx parser.cmx lexer.cmx symbol.cmx semantic.cmx print_fun.cmx address_records.cmx codegen.cmx main.cmx 

main.cmi main.cmx: main.ml 
	# ocamlc -c main.ml
	ocamlopt $(LLVM_FLAGS) main.ml

optimizer.cmx: optimizer.ml
	# ocamlfind ocamlopt -package llvm -linkpkg $(LLVM_FLAGS) optimizer.ml
	ocamlopt $(LLVM_FLAGS) optimizer.ml

codegen.cmx: codegen.ml ast.cmx address_records.cmx
	ocamlopt $(LLVM_FLAGS) codegen.ml

address_records.cmx: address_records.ml
	ocamlopt $(LLVM_FLAGS) address_records.ml

semantic.cmx: semantic.ml ast.cmx symbol.cmx lexer.cmi
	ocamlopt -c semantic.ml

parser.cmx: parser.ml lexer.cmi 
	ocamlopt -c parser.ml

lexer.cmi lexer.cmx: lexer.ml parser.cmi
	ocamlopt -c lexer.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.cmi: parser.mli
	ocamlopt -c parser.mli

parser.exe parser.ml parser.mli: parser.mly 
	ocamlyacc -v parser.mly 

symbol.cmx: symbol.ml ast.cmx
	ocamlopt -c symbol.ml

print_fun.cmx: print_fun.ml ast.cmx
	ocamlopt -c print_fun.ml

ast.cmx: ast.ml
	ocamlopt -c ast.ml

library:
	make -C $(LIBPATH)

clean:
	find . -name '*.cmi' -type f -delete
	find . -name '*.cmx' -type f -delete
	find . -name '*.cmo' -type f -delete
	find . -name '*.o' -type f -delete
	find . -name '*.mli' -type f -delete
	find . -name '*.output' -type f -delete
	find . -name 'parser.ml' -type f -delete
	find . -name 'lexer.ml' -type f -delete

distclean: clean
	find . -name 'main' -type f -delete
	find . -name '*.ll' -type f -delete
	find . -name '*.s' -type f -delete
	find . -name '*.out' -type f -delete
	find . -name 'edsger' -type f -delete

