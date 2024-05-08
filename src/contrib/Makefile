all: exomizer-3.0.2/src/exomizer obj/oscar64

exomizer-3.0.2/src/exomizer:
	cd exomizer-3.0.2/src && $(MAKE)

obj/oscar64:
	rm -f .passed-oscar64
	mkdir -p oscar64/build
	cd oscar64/build && $(MAKE) -f ../make/makefile

test: .passed-oscar64 .passed-unity

.passed-oscar64:
	cd oscar64/autotest && $(MAKE)
	touch .passed-oscar64

.passed-unity:
	cd unity/test && $(MAKE) test
	touch .passed-unity

clean:
	rm -f .passed-oscar64
	rm -f .passed-unity
	rm -rf oscar64/build
	cd unity/test && $(MAKE) clean
	cd exomizer-3.0.2/src && $(MAKE) clean
	cd oscar64/autotest && $(MAKE) clean
