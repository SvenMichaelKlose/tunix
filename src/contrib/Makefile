SUBDIRS = exomizer-3.0.2/src/exomizer cregex

ifdef HAVE_OSCAR64
SUBDIRS += obj/oscar64
endif

all: $(SUBDIRS)

exomizer-3.0.2/src/exomizer:
	cd exomizer-3.0.2/src && $(MAKE)

obj/oscar64:
	rm -f .passed-oscar64
	mkdir -p oscar64/build
	cd oscar64/build && $(MAKE) -f ../make/makefile

.passed-oscar64:
	cd oscar64/autotest && $(MAKE)
	touch .passed-oscar64

cregex:
	$(MAKE) -C cregex

test: .passed-oscar64 .passed-unity .passed-cregex

.passed-unity:
	cd unity/test && $(MAKE) test
	touch .passed-unity

.passed-cregex:
	cd cregex/test && $(MAKE)
	cregex/tests/driver || exit 1
	touch .passed-cregex

clean:
	rm -f .passed-oscar64
	rm -f .passed-unity
	rm -rf oscar64/build
	cd unity/test && $(MAKE) clean
	cd exomizer-3.0.2/src && $(MAKE) clean
	cd oscar64/autotest && $(MAKE) clean
