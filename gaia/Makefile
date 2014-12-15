
FRONT = urns-front
SIM = sim
TARGETS = $(FRONT) $(SIM)

FRONTFLAGS = -use-menhir
FRONTDIR = frontend

all: $(TARGETS)

$(FRONT): $(FRONTDIR)
	ocamlbuild $(FRONTDIR)/main.native -r -lib str -no-links $(FRONTFLAGS)
	cp _build/$(FRONTDIR)/main.native $@

$(SIM): gaiasim.c
	gcc gaiasim.c -o $@ -Wall -Wextra -O2 -std=c99 -lm

PHONY: clean
clean:
	rm -f $(TARGETS)
	rm -rf _build/
	rm -f *.out

