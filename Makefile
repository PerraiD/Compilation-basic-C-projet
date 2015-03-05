EXEC = compilateur

CAMLC = ocamlc
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

all:
	$(CAMLLEX) parser.mll
	$(CAMLYACC) patterns.mly
	$(CAMLC) -c patterns.mli
	$(CAMLC) -c patterns.ml
	$(CAMLC) -c parser.ml
	
	
	$(CAMLC) patterns.cmo parser.cmo main.ml -o $(EXEC)

clean:
	rm -f *.cm[iox] *.mli *~ .*~ *.o $(EXEC) prog patterns.ml parser.ml prog.c



