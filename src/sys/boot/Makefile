PROGRAM = flashboot.bin
ASMSOURCES = rom-info.asm start.asm
SOURCES = main.c
LIBRARIES = \
    $(TUNIX_LIB)/directory/libdirectory.a \
    $(TUNIX_LIB)/ultimem/libultimem.a
LD65CFG = vic.cfg
NO_EXOMIZER = 1

include ../../mk/Makefile.build
