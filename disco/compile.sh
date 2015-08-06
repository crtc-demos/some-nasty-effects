#!/bin/sh
set -e
pasta palswitch.s -o palswch
6502-gcc -mmach=bbcmaster -mcpu=65C02 -O2 disco.c -Wl,-D,__STACKTOP__=0x1fff -o demo -save-temps -Wl,-m,disco.map
cp -r demo demo.inf palswch palswch.inf "$OUTPUTDISK"
