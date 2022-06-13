all: main clean

main: ast ast.cmo lexer.cmo parser.cmo main.cmo 
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

parser.ml parser.mli: parser.mly 
	ocamlyacc parser.mly 

ast.cmo: ast.ml
	ocamlc -c ast.ml

ast: ast.ml
	ocamlopt -o ast ast.ml

clean: 
	rm ast.exe ast.cmi ast.cmx ast.o main.cmi main.cmo parser.cmi parser.cmo parser.mli parser.ml lexer.cmi lexer.cmo lexer.ml