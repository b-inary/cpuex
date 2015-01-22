
TARGETS = bin/urns-front bin/sim

FRONTFLAGS = -use-menhir
FRONTDIR = frontend

all: $(TARGETS)

bin/urns-front: $(FRONTDIR)
	ocamlbuild $(FRONTDIR)/main.native -r -lib str -no-links $(FRONTFLAGS)
	cp _build/$(FRONTDIR)/main.native $@

bin/sim: software/
	git submodule update --init
	$(MAKE) -C software/
	mv software/sim bin/sim

PHONY: clean
clean:
	rm -f $(TARGETS) *.out *.ll *.s
	rm -rf _build/

