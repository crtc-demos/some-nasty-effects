#!/bin/sh
set -e
pasta showimage.s -o showimg
pasta lz4test.s -o lz4test
# 6502-gcc -O0 -mmach=bbcmaster -mcpu=65C02 -Wl,-D,__STACKTOP__=0x2fff clz4.c -o clz4
cp -r frgdmp3.lz4 frgdmp3.lz4.inf \
      cham.lz4 cham.lz4.inf \
      mduck2.lz4 mduck2.lz4.inf \
      prrtdmp.lz4 prrtdmp.lz4.inf \
      nasty.lz4 nasty.lz4.inf \
      showimg showimg.inf "$OUTPUTDISK"
