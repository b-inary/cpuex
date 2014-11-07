
PROGRAM = urns-front

all: $(PROGRAM)

$(PROGRAM):
	ocamlbuild main.native -use-menhir -no-links
	cp _build/main.native $(PROGRAM)

.PHONY: clean
clean:
	rm -f $(PROGRAM)
	ocamlbuild -clean
 
