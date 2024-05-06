PROGRAM=flashboot.bin
ASMSOURCES=rom-info.asm start.asm
#ASMSOURCES=alloc.asm api.asm copy-bank.asm launch.asm main.asm save-state.asm rom-info.asm prgend.asm
SOURCES=main.c
LIBRARIES= \
    $(TUNIX_LIB)/directory-list/libdirectory-list.a \
    $(TUNIX_LIB)/ultimem/libultimem.a
LD65CFG=vic.cfg
NO_EXOMIZER=1

include ../Makefile.native
