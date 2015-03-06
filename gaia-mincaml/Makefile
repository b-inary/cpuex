
min-rt.out: min-rt.s bin/asm.py
	./bin/asm.py -f_end_of_program -o $@ -v globals.s lib.s min-rt.s

min-rt.s: min-rt.ml bin/min-caml
	./bin/min-caml min-rt.ml -inline 160 -log 0

bin/min-caml: src/
	ocamlbuild src/main.native -no-links
	cp -f _build/src/main.native $@

bin/asm.py: toolchain/
	git submodule update --init
	cp -f toolchain/asm.py $@

bin/sim: toolchain/
	$(MAKE) -C toolchain/
	cp -f toolchain/sim $@

PHONY: run
run: min-rt.out bin/sim
	./bin/sim -simple -stat min-rt.out < contest.bin > contest.ppm

PHONY: clean
clean:
	rm -f bin/* contest.ppm min-rt.out min-rt.out.s min-rt.s
	rm -rf _build/

