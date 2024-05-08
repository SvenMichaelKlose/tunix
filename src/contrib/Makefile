all: exomizer-3.0.2/src/exomizer obj/oscar64

exomizer-3.0.2/src/exomizer:
	cd exomizer-3.0.2/src && $(MAKE)

obj/oscar64:
	mkdir -p oscar64/build
	cd oscar64/build && $(MAKE) -f ../make/makefile

clean:
	rm -rf oscar64/build
	cd exomizer-3.0.2/src && $(MAKE) clean
