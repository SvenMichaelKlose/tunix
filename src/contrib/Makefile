include ../mk/Makefile.config

SUBDIRS = cc65/bin/cc65 cregex exomizer-3.0.2/src/exomizer

all: $(SUBDIRS)

exomizer-3.0.2/src/exomizer:
	$(MAKE) -C exomizer-3.0.2/src

cc65/bin/cc65:
	$(MAKE) -C cc65

cregex:
	$(MAKE) -C cregex

test: .passed-cc65 .passed-unity .passed-cregex

.passed-cc65:
	$(MAKE) -C cc65 test
	touch .passed-cc65

.passed-cregex:
	$(MAKE) -C cregex check
	touch .passed-cregex

.passed-unity:
	$(MAKE) -C unity/test test
	touch .passed-unity

clean:
	rm -f .passed-cc65
	rm -f .passed-cregex
	rm -f .passed-unity
	$(MAKE) -C cc65 clean
	$(MAKE) -C unity/test clean
	$(MAKE) -C exomizer-3.0.2/src clean
