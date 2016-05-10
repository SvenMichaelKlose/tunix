all:
	$(CC) -o make-image make-image.c
	mkdir -pv compiled
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	./make-image
	rm -fv g.zip compiled/charset.lst
	cp -v README.md compiled
	cd compiled && zip -r ../g.zip *

clean:
	$(MAKE) -C src clean
	rm -frv g.zip compiled make-image
