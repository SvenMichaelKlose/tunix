PROGRAM=ultifs.prg
SOURCES= \
	alloc.c \
	ipc.c \
	kernal.c \
	main.c \
	test.c \
	ultifs.c
ASMSOURCES= \
	reset.asm \
	dispatcher.asm \
	zeropage.asm \
	wedge-secondary.asm
LIBRARIES= \
    $(TUNIX_LIB)/ultimem/libultimem.a \
    $(TUNIX_LIB)/posix/libposix.a \
    $(TUNIX_LIB)/log/liblog.a
LD65CFG=vic.cfg

include ../../Makefile.native
