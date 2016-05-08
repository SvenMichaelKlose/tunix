#!/bin/sh

set -e

for i in main; do \
    cc65 -O -Os -t vic20g -I /usr/local/share/cc65/include/ $i.c; \
    ca65 -I /usr/local/share/cc65/asminc/ $i.s; \
done

ld65 -C /usr/local/share/cc65/cfg/vic20g.cfg -o menu-c.bin main.o /usr/local/share/cc65/lib/vic20g.lib
