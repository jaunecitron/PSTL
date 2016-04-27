EXEC=test_cube.byte test_pyramide.byte test_boulon.byte test_cylindre.byte test_cone.byte bezier.byte test_sierpinski.byte
SRC=src/
BIN=bin/
all: $(EXEC)

%.byte:
	ocamlbuild -I $(SRC) $@
	mv $@ $(BIN)

clean:
	ocamlbuild -clean
	rm -f bin/*
