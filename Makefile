all:
	mkdir -pv compiled
	sbcl --noinform --core bender/bender gencode.lisp
	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	sbcl --noinform --core bender/bender make-image.lisp
	rm -fv g.zip compiled/charset.lst
	cp -v README.md compiled
	cd compiled && zip -r ../g.zip *

clean:
	$(MAKE) -C src clean
	rm -rv g.zip compiled
