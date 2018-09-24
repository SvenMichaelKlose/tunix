all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv g.zip
	rm -rfv compiled
	mkdir -pv compiled
	cp src/flashmenu/flashmenu.bin compiled/boot
	#cp -r archive/* compiled
	cp *.prg compiled
	touch compiled/FNORD
	./mkfs/mkfs.ultifs g.img n l src/flashboot/flashboot.bin i compiled w

clean:
	$(MAKE) -C src clean
	rm -frv g.zip compiled make-image
