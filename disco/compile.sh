#!/bin/sh
set -e
pasta palswitch.s -o palswch
6502-gcc -mmach=bbcmaster -mcpu=65C02 -O2 disco.c -Wl,-D,__STACKTOP__=0x26ff -o disco -save-temps -Wl,-m,disco.map
cp -r disco disco.inf palswch palswch.inf "$OUTPUTDISK"
