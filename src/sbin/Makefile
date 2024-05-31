include ../Makefile.config

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
SUBDIRS = fstest
endif

ifeq ($(TARGET), vic20)
SUBDIRS += ultiburn ultidump ultitest
endif

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
include ../Makefile.subdir
endif
