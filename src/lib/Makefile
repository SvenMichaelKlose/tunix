-include ../config
ifndef TARGET
TARGET=unix
endif
include ../mk/Makefile.targets

SUBDIRS = bdb simpleio simpleio-stdlib
$(info COMMODORE_TARGET = $(COMMODORE_TARGETS))
$(info TARGET = $(TARGET))

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
SUBDIRS += blit conio directory ingle log posix simpleio-cbm tunix zp lineedit text
$(info SUBDIRS = $(SUBDIRS))
endif

ifeq ($(TARGET), vic20)
SUBDIRS += gfx gui keyboard term term-test ultifs ultimem
endif

SUBDIRS += lisp

include ../mk/Makefile.subdir
