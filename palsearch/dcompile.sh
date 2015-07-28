#!/bin/sh
set -e
pasta showimage.s -o showimg
cp -r showimg showimg.inf "$OUTPUTDISK"
