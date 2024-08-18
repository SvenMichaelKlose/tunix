#!/bin/sh

printf "Writing to 'empty-ultimem.img'...\n"
dd if=/dev/zero bs=1024 count=8192 of="empty-ultimem.img"
