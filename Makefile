all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv ingle.zip
	rm -rfv compiled
	mkdir -pv compiled/.ingle
	cp -rv archive/* compiled
	cp -v src/desktop/*.bin compiled/.ingle/
	./mkfs/mkfs.ultifs ingle.img n l src/flashboot/flashboot.bin i compiled w
	./mkfs/mkfs.ultifs image n l src/flashboot/flashboot.bin i compiled W
	rm -fv tmp.prg
	rm -rfv native-pkgs
	mkdir -p native-pkgs
	cp src/vi/vi.zip native-pkgs/
	rm -rfv bin
	mkdir -p bin
	cp src/fstest/fstest bin/
	cp src/sjload/sjload bin/
	cp src/ultiburn/ultiburn bin/
	cp src/ultidump/ultidump bin/
	cp src/ultifs/ultifs bin/
#	cp src/ultitest/ultitest bin/
	cp src/vi/README.md bin/vi.md
	cp src/vi/vi bin/

clean:
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	rm -fr ingle.img ingle.zip compiled image ingledata.bin ingleinstall ingle.img.zip native-pkgs bin
