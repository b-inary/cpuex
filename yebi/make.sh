#!/bin/sh

option="-inline 64"

cd compiler/
make
cd ..
echo ./compiler/min-caml min-rt.ml $option
./compiler/min-caml min-rt.ml $option
sed -e "/main:/a \    call    init_globals" min-rt.s > min-rt_2.s
mv min-rt_2.s min-rt.s
./asm.py min-rt.s globals.s -s

