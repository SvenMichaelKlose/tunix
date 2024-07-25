include ../mk/Makefile.config

SUBDIRS = cc65/bin/cc65 cregex exomizer-3.0.2/src/exomizer

ifdef HAVE_OSCAR64
SUBDIRS += obj/oscar64
endif

all: $(SUBDIRS)

exomizer-3.0.2/src/exomizer:
	$(MAKE) -C exomizer-3.0.2/src

obj/oscar64:
	rm -f .passed-oscar64
	mkdir -p oscar64/build
	$(MAKE) -C oscar64/build -f ../make/makefile

cc65/bin/cc65:
	$(MAKE) -C cc65 all

cregex:
	$(MAKE) -C cregex

test: .passed-cc65 .passed-oscar64 .passed-unity .passed-cregex

.passed-cc65:
	$(MAKE) -C cc65 test
	touch .passed-cc65

.passed-cregex:
	$(MAKE) -C cregex check
	touch .passed-cregex

.passed-oscar64:
ifdef HAVE_OSCAR64
	$(MAKE) -C oscar64/autotest
endif
	touch .passed-oscar64

.passed-unity:
	$(MAKE) -C unity/test test
	touch .passed-unity

clean:
	rm -f .passed-cc65
	rm -f .passed-cregex
	rm -f .passed-oscar64
	rm -f .passed-unity
	rm -rf oscar64/build
	$(MAKE) -C cc65 clean
	$(MAKE) -C unity/test clean
	$(MAKE) -C exomizer-3.0.2/src clean
	$(MAKE) -C oscar64/autotest clean
