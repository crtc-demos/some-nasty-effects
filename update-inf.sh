#!/bin/bash

INF="$1"

if [ ! "$INF" ]; then
  echo "Inf file missing."
  exit 1
fi

CONTENTS="$(cat "$INF")"

set $CONTENTS

filename=$1
load=0x$2
exec=0x$3
size=0x$4
attr=$5
type=$6

if ! [ "$attr" ]; then
  attr="ATTR=0"
fi

if ! [ "$type" ]; then
  type="TYPE=1"
fi

orig_filename="$(basename "$INF" .inf)"

pushd "$(dirname "$INF")" >& /dev/null
actual_size="$(stat -c "%s" "$orig_filename")"
popd >& /dev/null

printf "$filename %.6X %.6X %.6X $attr $type\n" $load $exec $actual_size > "$INF".new
rm "$INF"
mv -f "$INF".new "$INF"
