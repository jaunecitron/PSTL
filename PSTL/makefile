OCAMLDEP=ocamldep
CAML=ocamlc
SRC=src/
OBJ=obj/
SIG=SIG/
INCLUDE=include/
BIN=bin/
EXEC=$(BIN)test-pyramide $(BIN)test-boulon $(BIN)test-bezier

SOURCE_CMO_POLYGONE= $(SRC)point.cmo $(SRC)instruction.cmo $(SRC)environment.cmo $(SRC)program.cmo $(SRC)droite.cmo $(SRC)segment.cmo $(SRC)polygone.cmo 
SOURCE_CMO_PYRAMIDE= $(SOURCE_CMO_POLYGONE) $(SRC)test_pyramide.cmo
SOURCE_CMO_BOULON= $(SOURCE_CMO_POLYGONE) $(SRC)test_boulon.cmo
SOURCE_CMO_BEZIER=$(SRC)point.cmo $(SRC)instruction.cmo $(SRC)environment.cmo $(SRC)program.cmo $(SRC)bezier.cmo



all: $(EXEC)

$(BIN)test-pyramide: $(SOURCE_CMO_PYRAMIDE)
	$(CAML) -I $(SRC) $? -o $@

$(BIN)test-boulon: $(SOURCE_CMO_BOULON)
	$(CAML) -I $(SRC) $? -o $@

$(BIN)test-bezier: $(SOURCE_CMO_BEZIER)
	$(CAML) -I $(SRC) $? -o $@

$(SRC)%.cmo: $(SRC)%.ml
	$(CAML) -I $(SRC) -c $?


#CLEAN
clean:
	rm -f $(SRC)*.cmo $(SRC)*.cmi $(SRC)#* $(SRC)*~ $(EXEC)


#DEPENDENCIES
depend:
	$(OCAMLDEP) -I $(SRC) -native $(SRC)*.ml > .depend

include .depend
