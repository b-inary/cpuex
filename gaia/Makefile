
TARGET = min-rt.opt.ll
BIN = bin/urns-front bin/sim

all: $(TARGET) $(BIN)

$(TARGET): bin/urns-front
	bin/urns-front globals.ml min-rt.ml
	bin/opt.sh min-rt.ll

bin/urns-front: frontend/
	ocamlbuild frontend/main.native -r -lib str -no-links -use-menhir
	cp _build/frontend/main.native $@

bin/sim: software/
	git submodule update --init
	$(MAKE) -C software/
	mv software/sim bin/sim

native: bin/urns-front
	bin/urns-front globals.ml min-rt.ml
	bin/native.sh min-rt.ll
	time -p ./min-rt.out < contest.bin > contest.ppm

PHONY: clean
clean:
	rm -f $(BIN) *.ll *.s *.out *.ppm
	rm -rf _build/

