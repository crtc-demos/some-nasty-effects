#!/bin/bash
for l in `seq 0 24`; do
  if [ $l -eq 9 ]; then
    ldup=8
  elif [ $l -lt 13 ] ; then
    ldup=$l
  elif [ $l -gt 13 ] ; then
    ldup=$(( l - 1 ))
  else
    ldup=BLANK
  fi
  if [ $ldup = "BLANK" ]; then
    printf " \x9d                                     \x9c"
  else
    dd if=endscreen.m7.orig bs=1 count=40 skip=$(( ldup * 41 ))
  fi
done > endscreen.m7
