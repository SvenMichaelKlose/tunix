-include ../../config

PROGRAM = lisp
SOURCES = main.c
LIBRARIES = \
    $(TUNIX_LIB)/lisp/liblisp.a \
    $(TUNIX_LIB)/simpleio/libsimpleio.a
CLEANFILES = image test.out doc/manual.pdf

ifndef NAIVE
SOURCES += test.c
endif

ifeq ($(TARGET), cpm)
LIBRARIES += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
endif
ifneq (,$(filter $(TARGET), $(CC65_TARGETS)))
LIBRARIES += $(TUNIX_LIB)/simpleio-cbm/libsimpleio-cbm.a
endif
ifeq ($(TARGET), unix)
LIBRARIES += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
endif

LD65CFG = cc65-cfg/$(CC65_TARGET).cfg

include ../../mk/Makefile.build
