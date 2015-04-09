EXEC = compilateur

CAMLC = ocamlc
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

all:
	$(CAMLC) -c arbre.ml
	$(CAMLLEX) lexer.mll
	$(CAMLYACC) parser.mly
	$(CAMLC) -c parser.mli
	$(CAMLC) -c parser.ml
	$(CAMLC) -c lexer.ml
	
	
	$(CAMLC) arbre.cmo parser.cmo lexer.cmo main.ml -o $(EXEC)

clean:
	rm -f *.cm[iox] *.mli *~ .*~ *.o $(EXEC) prog parser.ml lexer.ml prog.c



