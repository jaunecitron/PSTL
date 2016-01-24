CAML=ocamlc
SRC=src/
OBJ=obj/
LIB=lib/
INCLUDE=include/
BIN=bin/
EXEC=$(BIN)test

SOURCE_ML=$(SRC)instruction.ml $(SRC)env.ml $(SRC)program.ml 
SOURCE_MLI=$(LIB)instruction.mli $(LIB)env.mli $(LIB)program.mli
SOURCE_CMI=$(INCLUDE)instruction.cmi $(INCLUDE)env.cmi $(INCLUDE)program.cmi
SOURCE_CMO=$(OBJ)instruction.cmo $(OBJ)env.cmo $(OBJ)program.cmo


all: $(EXEC)

$(OBJ)%.cmo: $(INCLUDE)%.cmi $(SRC)%.ml
	$(CAML) -o $@ -c $?
$(INCLUDE)%.cmi: $(LIB)%.mli
	$(CAML) -o $@ -c $?
$(EXEC): $(SOURCE_CMO)
	$(CAML) -o $@ -c $?

clean:
	rm $(INCLUDE)*.cmi $(OBJ)*.cmo $(SRC)*~ $(EXEC) 
