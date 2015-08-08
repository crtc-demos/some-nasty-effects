#!/bin/sh
set -e
./conv.sh
cp endscreen.m7 endscreen.m7.inf "$OUTPUTDISK"
