all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv g.zip
	rm -rfv compiled
	mkdir -pv compiled
	cp src/flashmenu/flashmenu.bin compiled/boot
	cp src/flashmenu/desktop.bin compiled/desktop
	cp src/flashmenu/ultifs.bin compiled/ultifs
	cp -r archive/* compiled
	./mkfs/mkfs.ultifs g.img n l src/flashboot/flashboot.bin i compiled w

clean:
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	rm -frv g.img g.zip compiled make-image
