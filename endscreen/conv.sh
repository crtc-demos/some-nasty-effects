#!/bin/bash
for l in `seq 0 25`; do
  if [ $l -eq 9 ]; then
    ldup=8
  elif [ $l -lt 16 ] ; then
    ldup=$l
  else
    ldup=$(( l - 1 ))
  fi
  dd if=endscreen.m7.orig bs=1 count=40 skip=$(( ldup * 41 ))
done > endscreen.m7
