#!/bin/sh
./palsearch input18.png -o mduckX >& mduck.log &
./palsearch input2.png -o frogX >& frog.log &
./palsearch input6.png -o parrotX >& parrot.log &
wait
