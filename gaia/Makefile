
FRONT = urns-front
TARGETS = $(FRONT)

FRONTFLAGS = -use-menhir
FRONTDIR = uranus-frontend

all: $(TARGETS)

$(FRONT): $(FRONTDIR)
	ocamlbuild $(FRONTDIR)/main.native -r -lib str -no-links $(FRONTFLAGS)
	cp _build/$(FRONTDIR)/main.native $@

PHONY: clean
clean:
	rm -f $(TARGETS)
	rm -rf _build/
