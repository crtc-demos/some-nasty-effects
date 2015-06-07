#!/bin/sh
set -e
pasta showimage.s -o showimg
cp -r showimg showimg.inf dump dump.inf prrtdmp prrtdmp.inf frogdmp frogdmp.inf frgdmp2 frgdmp2.inf beach beach.inf "$OUTPUTDISK"
