all: main clean

main: lexer.cmo parser.cmo main.cmo
	ocamlc -o main lexer.cmo parser.cmo main.cmo

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

clean: 
	rm main.cmi main.cmo parser.cmi parser.cmo parser.mli parser.ml lexer.cmi lexer.cmo lexer.ml