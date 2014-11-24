
PROGRAM = urns-front
OPTION  = -use-menhir

all: $(PROGRAM)

$(PROGRAM): *.ml *.mll *.mly
	ocamlbuild main.native -lib str -no-links $(OPTION)
	cp _build/main.native $(PROGRAM)

.PHONY: clean
clean:
	rm -f $(PROGRAM)
	rm -rf _build

