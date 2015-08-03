#!/bin/sh
set -e
ocamlc mksintab.ml -o mksintab
./mksintab > sintab.s
ocamlc mkringtab.ml -o mkringtab
./mkringtab > ringtab.s
$PASTA ringtab.s -o rings
lz4 -9 -f rings
$PASTA -D EMULATOR_MODE chunky.s -o demo
6502-gcc -mmach=bbcmaster -mcpu=65C02 -O2 paltest.c -Wl,-D,__STACKTOP__=0x2fff -o paltest -save-temps
cp -r demo demo.inf rings.lz4 rings.lz4.inf "$OUTPUTDISK"
