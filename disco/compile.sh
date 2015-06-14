#!/bin/sh
set -e
6502-gcc -mmach=bbcmaster -mcpu=65C02 -Os disco.c -Wl,-D,__STACKTOP__=0x2fff -o disco -save-temps -Wl,-m,disco.map
cp -r disco disco.inf "$OUTPUTDISK"
