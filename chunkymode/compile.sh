#!/bin/sh
set -e
ocamlc mksintab.ml -o mksintab
./mksintab > sintab.s
$PASTA chunky.s -o demo
6502-gcc -mmach=bbcmaster -mcpu=65C02 -O2 paltest.c -Wl,-D,__STACKTOP__=0x2fff -o paltest -save-temps
cp -r demo demo.inf paltest paltest.inf "$OUTPUTDISK"
