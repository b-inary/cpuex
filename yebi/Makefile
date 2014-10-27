OPTION = -inline 112

all: compiler min-rt

compiler:
	$(MAKE) -C $@

min-rt:
	./compiler/min-caml min-rt.ml $(OPTION)
	sed -ie '/^main:/a\    call    init_globals' min-rt.s
	./asm.py -s min-rt.s globals.s
