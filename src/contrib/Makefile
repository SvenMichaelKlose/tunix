include ../mk/Makefile.config

SUBDIRS = exomizer-3.0.2/src/exomizer cregex

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

.passed-oscar64:
ifdef HAVE_OSCAR64
	$(MAKE) -C oscar64/autotest
endif
	touch .passed-oscar64

cregex:
	$(MAKE) -C cregex

test: .passed-oscar64 .passed-unity .passed-cregex

.passed-unity:
	$(MAKE) -C unity/test test
	touch .passed-unity

.passed-cregex:
	$(MAKE) -C cregex check
	touch .passed-cregex

clean:
	rm -f .passed-cregex
	rm -f .passed-oscar64
	rm -f .passed-unity
	rm -rf oscar64/build
	$(MAKE) -C unity/test clean
	$(MAKE) -C exomizer-3.0.2/src clean
	$(MAKE) -C oscar64/autotest clean
