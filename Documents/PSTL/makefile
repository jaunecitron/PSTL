CAMLC=ocamlc
COMPFLAGS= -i
FILES= t1.cmo t2.cmo t3.cmo
FILEEXE=test
BIN=bin/
SRC=src/
LIB=lib/
INCLUDE=include/


all: $(FILES)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

$(CAMLC) -o $(FILEEXE) $(FILES)

.ml.cmo:
$(CAMLC) $(COMPFLAGS) -c $<
.mli.cmi:
$(CAMLC) $(COMPFLAGS) -c $<
