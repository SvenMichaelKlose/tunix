include ../config

SUBDIRS = bdb lisp simpleio simpleio-stdlib

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
SUBDIRS += blit conio directory-list ingle lisp log posix simpleio-cbm tunix zp lineedit text
endif

ifeq ($(TARGET), vic20)
SUBDIRS += gfx gui keyboard term term-test ultifs ultimem
endif

include ../mk/Makefile.subdir
