CAML=ocamlc
SRC=src/
OBJ=obj/
SIG=SIG/
INCLUDE=include/
BIN=bin/
EXEC=$(BIN)test-basic $(BIN)test-bezier

SOURCE_CMO=$(SRC)point.cmo $(SRC)instruction.cmo $(SRC)environment.cmo $(SRC)program.cmo $(SRC)polygone.cmo $(SRC)test.cmo
SOURCE_CMO_2=$(SRC)point.cmo $(SRC)instruction.cmo $(SRC)environment.cmo $(SRC)program.cmo $(SRC)bezier.cmo

all: $(EXEC)

$(BIN)test-basic: $(SOURCE_CMO)
	$(CAML) $? -o $@

$(BIN)test-bezier: $(SOURCE_CMO_2)
	$(CAML) $? -o $@

$(SRC)%.cmo: $(SRC)%.ml
	$(CAML) -I $(SRC) -c $?

clean:
	rm -f $(SRC)*.cmo $(SRC)*.cmi $(SRC)*~ $(EXEC) 
