-include ../../config
include ../../mk/Makefile.targets

ifndef TARGET
TARGET = unix	# TODO: Remove doublettes like this.
endif

PROGRAM 	= lisp
SOURCES 	= builtins.c main.c test.c
CLEANFILES 	= image test.out doc/manual.pdf git-version.lisp
LIBRARIES 	= $(TUNIX_LIB)/lisp/liblisp.a
ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
LIBRARIES  += $(TUNIX_LIB)/simpleio-cbm/libsimpleio-cbm.a
endif
ifneq (,$(filter $(TARGET), cpm sim6502 unix $(APPLE_TARGETS) $(ATARI_TARGETS)))
LIBRARIES  += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
endif
LIBRARIES  += $(TUNIX_LIB)/simpleio/libsimpleio.a

ifdef OVERLAY
LD65CFG = cc65-cfg/$(CC65_TARGET)-overlay.cfg
else
LD65CFG = cc65-cfg/$(CC65_TARGET).cfg
endif

include ../../mk/Makefile.build

CFLAGS += $(LISP_FLAGS)
