all: main clean

main: ast ast.cmo lexer.cmo parser.cmo main.cmo parser.exe
	ocamlc -o main lexer.cmo ast.cmo parser.cmo main.cmo 

main.cmi main.cmo: main.ml 
	ocamlc -c main.ml

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

ast.cmo: ast.ml
	ocamlc -c ast.ml

ast: ast.ml
	ocamlopt -o ast ast.ml

clean: 
	rm parser.output parser.ml parser.cmo parser.cmi parser.mli ast.cmi ast.cmo main.cmi main.cmo lexer.cmi lexer.cmo lexer.ml