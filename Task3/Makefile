.PHONY: pack all run clean
GENERATED=src/parser.mli src/parser.ml src/lexer.ml
SOURSE=freevars.ml unification.ml typeinfer.ml
OCAMLC=ocamlc
PACKAGE=hw3.zip

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run:
	./main.exe   

compParser:
	cd src && $(OCAMLC) $(GENERATED:src/%=%)

compGrammar:
	cd src && $(OCAMLC) -c grammar.ml

compSource:
	cd src && $(OCAMLC) -c $(SOURSE)    
    
main.exe: $(GENERATED) compGrammar compParser compSource
	cd src && $(OCAMLC) grammar.ml $(GENERATED:src/%=%) $(SOURSE) main.ml -o ../main.exe

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src

$(GENERATED): src/lexer.mll src/parser.mly
	ocamllex src/lexer.mll 
	ocamlyacc src/parser.mly

clean:
	cd src && $(DEL) camlprog.exe $(GENERATED:src/%=%) *.c* *.o*
	$(DEL) main.exe $(PACKAGE)
