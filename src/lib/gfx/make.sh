#!/bin/sh

set -e

FILES="box calcscr clear-screen clip compress-font end frame hline init init-bitmap-mode masks patterns putchar putstring reset-region start vfill vline zeropage"
rm *.o

for i in $FILES; do \
    echo $i.asm
    ca65 -I /usr/local/share/cc65/asminc/ $i.asm; \
done

ar65 a libgfx.a *.o
