include Makefile.config

SUBDIRS = contrib lib bin

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
SUBDIRS += sbin
endif

ifeq ($(TARGET), vic20)
SUBDIRS += drv sys
endif

include Makefile.subdir
