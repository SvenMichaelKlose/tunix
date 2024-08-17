-include ../config
ifndef TARGET
TARGET=unix
endif

SUBDIRS = csv2html lisp
ifeq ($(TARGET), vic20)
SUBDIRS += cbm-console desktop vi vm
endif

include ../mk/Makefile.subdir
