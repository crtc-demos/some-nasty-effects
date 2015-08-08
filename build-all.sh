#!/bin/bash -x

cd $(dirname "$0")

rm -rf demodisk.ssd

if [ -x "$(which pasta)" ]; then
  PASTA=$(which pasta)
else
  PASTA=/home/jules/code/pasta/pasta
fi

if [ -x "$(which bbcim)" ]; then
  BBCIM=$(which bbcim)
else
  if ! [ -x bbcim/bbcim ]; then
    pushd bbcim
    ./mkbbcim
    popd
  fi
  BBCIM=$(readlink -f bbcim/bbcim)
fi

if [ ! -x "$PASTA" ] || [ ! -x "$BBCIM" ]; then
  echo 'Missing pasta or bbcim! Whoops.'
  exit 1
fi

VGMPROC="$(readlink -f ../mode-infinity/vgmproc/vgmproc)"
if [ ! -x "$VGMPROC" ]; then
  echo "vgmproc missing from $VGMPROC (build mode-infinity)"
  exit 1
fi

export PASTA
export BBCIM
export VGMPROC

DEMONAME=somenastyeffects

OUTPUTDISK="$(readlink -f tmpdisk)"

mkdir -p "$OUTPUTDISK"
pushd "$OUTPUTDISK"
rm -f *
popd

export OUTPUTDISK

set -e

pushd chunkymode
./compile.sh
popd

pushd palsearch
./dcompile.sh
popd

pushd disco
./compile.sh
popd

#pushd phasetest
#./compile.sh
#popd

pushd scroll
./compile.sh
popd

pushd vgmplayer
./compile.sh
popd

pushd endscreen
./compile.sh
popd

rm -f '!boot'
unix2mac -n 'boot.in' '!boot'

cp -f '!boot' '!boot.inf' "$OUTPUTDISK"

find . -name "*.inf" -exec ./update-inf.sh {} \;

$BBCIM -new demodisk.ssd
pushd tmpdisk
$BBCIM -a ../demodisk.ssd *
popd
# $BBCIM -80 demodisk.ssd

rm -rf "$DEMONAME"
mkdir "$DEMONAME"
cp demodisk.ssd "$DEMONAME"/"$DEMONAME".ssd
cp README "$DEMONAME"/README
tar fcvz "$DEMONAME".tar.gz "$DEMONAME"
