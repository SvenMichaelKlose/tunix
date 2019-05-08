all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv g.zip
	rm -rfv compiled
	mkdir -pv compiled/g
	cp src/desktop/*.bin compiled/g/
	cp src/tgi-demo/*.bin compiled/g/
	cp -r archive/* compiled
	cp README.md compiled
	./mkfs/mkfs.ultifs g.img n l src/flashboot/flashboot.bin i compiled w
	./mkfs/mkfs.ultifs gdata.bin n l src/flashboot/flashboot.bin i compiled W
	cp src/installer/ginstall .

clean:
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	rm -frv g.img g.zip compiled make-image
