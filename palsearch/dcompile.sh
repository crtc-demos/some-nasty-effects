#!/bin/sh
set -e
pasta showimage.s -o showimg
cp -r showimg showimg.inf dump dump.inf frgdmp2 frgdmp2.inf "$OUTPUTDISK"
