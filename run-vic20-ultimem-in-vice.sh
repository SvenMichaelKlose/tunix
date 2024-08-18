#!/bin/sh

dd if=/dev/zero bs=1024 count=8192 of="empty-ultimem.img"
xvic -ultimem tunix.img $@
