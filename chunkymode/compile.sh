#!/bin/sh
set -e
$PASTA chunky.s -o demo
cp -r demo demo.inf "$OUTPUTDISK"
