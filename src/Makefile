include mk/Makefile.config

SUBDIRS = lib bin

ifneq (,$(filter $(TARGET), $(COMMODORE_TARGETS)))
SUBDIRS += sbin
endif

ifeq ($(TARGET), vic20)
SUBDIRS += drv sys
endif

include mk/Makefile.subdir

host:
	$(MAKE) -C contrib

hosttest:
	$(MAKE) -C contrib test

hostclean:
	$(MAKE) -C contrib clean
