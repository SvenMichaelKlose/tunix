all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv g.zip
	rm -rfv compiled
	mkdir -pv compiled
	cp src/flashmenu/flashmenu.bin compiled/boot
	./mkfs/mkfs.ultifs compiled/ultimem.img n l src/flashboot/flashboot.bin i compiled w
	cd compiled && zip -r ../g.zip *

clean:
	$(MAKE) -C src clean
	rm -frv g.zip compiled make-image
