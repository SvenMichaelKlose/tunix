include ../config

SUBDIRS = bdb lisp simpleio simpleio-stdlib

ifneq ($(TARGET), unix)
SUBDIRS += blit conio directory-list ingle lisp log posix simpleio-cbm tunix zp lineedit text
endif

ifeq ($(TARGET), vic20)
SUBDIRS += gfx gui keyboard term term-test ultifs ultimem
endif

include ../Makefile.subdir
