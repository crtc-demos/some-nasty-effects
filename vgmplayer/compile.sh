#!/bin/sh
set -e
# $VGMPROC "BotB 5106 null1024 - iNTeNSiTY.vgm" intens.s
# $PASTA intens.s -o intens
# $VGMPROC "BotB 16439 Chip Champion - frozen dancehall of the pharaoh.vgm" froz.s
# $PASTA froz.s -o froz
# $VGMPROC "BotB 14647 serbian-caves.vgm" serbian.s
# $PASTA serbian.s -o serbian
$VGMPROC "BotB 4406 sonic_enters_a_dance_club.vgm" sonic.s
$PASTA sonic.s -o sonic
$PASTA player.s -o player
#cp -r player player.inf intens intens.inf serbian serbian.inf "$OUTPUTDISK"
cp -r player player.inf sonic sonic.inf "$OUTPUTDISK"
