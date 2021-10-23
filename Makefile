all:
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp
#	cd src/sh && ./make.sh && cd ../.. && cp src/sh/sh compiled
	$(MAKE) -C src all
	$(MAKE) -C mkfs all
	rm -fv ingle.zip
	rm -rfv compiled
	mkdir -pv compiled/.ingle
	cp -rv archive/* compiled
	cp -v src/desktop/*.bin compiled/.ingle/
#	mkdir -pv compiled/programming/attolisp
	cp -v src/lisp/lisp compiled/
	mkdir -pv compiled/office
	cp -v src/writenow/writenow compiled/office/writenow/
	cp -v src/vforth/vforth compiled/programming/vforth/
	cp -v src/vi/vi compiled/
#	cp -v src/tgi-demo/*.bin compiled/.ingle/
	./mkfs/mkfs.ultifs ingle.img n l src/flashboot/flashboot.bin i compiled w
	./mkfs/mkfs.ultifs ingledata.bin n l src/flashboot/flashboot.bin i compiled W
	rm -fv tmp.prg
	cp src/installer/ingle .

clean:
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	rm -frv ingle.img ingle.zip compiled make-image ingledata.bin ingleinstall
