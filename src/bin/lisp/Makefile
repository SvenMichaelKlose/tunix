-include ../../config

PROGRAM = lisp
SOURCES = main.c
LIBRARIES = \
    $(TUNIX_LIB)/lisp/liblisp.a \
    $(TUNIX_LIB)/simpleio/libsimpleio.a

ifeq ($(TARGET), unix)
LIBRARIES += $(TUNIX_LIB)/simpleio-stdlib/libsimpleio-stdlib.a
else
LIBRARIES += $(TUNIX_LIB)/simpleio-cbm/libsimpleio-cbm.a
endif
LD65CFG = cfg/$(CC65_TARGET).cfg

include ../../mk/Makefile.build
