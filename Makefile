all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	mkdir -pv compiled/.ingle
	cp -r archive/* compiled
#	cp src/desktop/*.bin compiled/.ingle/
	./mkfs/mkfs.ultifs ingle.img n l src/flashboot/flashboot.bin i compiled w
	./mkfs/mkfs.ultifs image n l src/flashboot/flashboot.bin i compiled W
	mkdir -p native-pkgs
	cp src/vi/vi.zip native-pkgs/
	mkdir -p bin
	cp src/fstest/fstest bin/
	cp src/ultiburn/ultiburn bin/
	cp src/ultidump/ultidump bin/
	cp src/ultifs/ultifs bin/
	cp src/ultitest/ultitest bin/
	cp src/vi/README.md bin/vi.md
	cp src/vi/vi bin/
	c1541 -format "fstest,01" d64 test.d64 -write src/ultifs/ultifs -write src/fstest/fstest

clean:
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	rm -fv ingle.zip
	rm -rfv compiled
	rm -fv tmp.prg
	rm -fr ingle.img ingle.zip compiled image ingledata.bin ingleinstall ingle.img.zip native-pkgs/* bin/*
