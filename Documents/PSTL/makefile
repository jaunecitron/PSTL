CAML=ocamlc
SRC=src/
OBJ=obj/
LIB=lib/
INCLUDE=include/
BIN=bin/
EXEC=$(BIN)test

SOURCE_CMO=$(SRC)point.cmo $(SRC)instruction.cmo $(SRC)environment.cmo $(SRC)program.cmo $(SRC)polygone.cmo $(SRC)test.cmo


all: $(EXEC)

$(SRC)%.cmo: $(SRC)%.ml
	$(CAML) -I $(SRC) -c $?

$(EXEC): $(SOURCE_CMO)
	$(CAML) $? -o $@

clean:
	rm $(SRC)*.cmo $(SRC)*.cmi $(EXEC) 
