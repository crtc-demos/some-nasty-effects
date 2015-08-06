#!/bin/sh
if [ -e /home/jules/stuff-in-hiding/gcc-6502-bits ]; then
  export PATH=/home/jules/stuff-in-hiding/gcc-6502-bits/prefix/bin:$PATH
elif [ -e /home/jules/code/gcc-6502-bits ]; then
  export PATH=/home/jules/code/gcc-6502-bits/prefix/bin:$PATH
fi
