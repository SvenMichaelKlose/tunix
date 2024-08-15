-include ../../config
include ../../mk/Makefile.targets

PROGRAM = lisp
SOURCES = builtins.c main.c
LIBRARIES = \
    $(TUNIX_LIB)/lisp/liblisp.a \
    $(TUNIX_LIB)/simpleio/libsimpleio.a
CLEANFILES = image test.out doc/manual.pdf

ifdef TEST
SOURCES += test.c
endif

ifeq ($(TARGET), cpm)
LIBRARIES += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
endif
ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
LIBRARIES += $(TUNIX_LIB)/simpleio-cbm/libsimpleio-cbm.a
endif
ifeq ($(TARGET), unix)
LIBRARIES += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
endif

ifdef OVERLAY
LD65CFG = cc65-cfg/$(CC65_TARGET)-overlay.cfg
else
LD65CFG = cc65-cfg/$(CC65_TARGET).cfg
endif

include ../../mk/Makefile.build

CFLAGS += $(LISP_FLAGS)
