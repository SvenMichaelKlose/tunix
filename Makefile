all: world mkfs/mkfs.ultifs ultimem_image c1541_image
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

world:
	@echo "# Making world."
	$(MAKE) -C src all

test:
	$(MAKE) -C src test

mkfs/mkfs.ultifs:
	@echo "# Making host mkfs."
	$(MAKE) -C mkfs all

ultimem_image:
	@echo "# Making UltiMem ROM image."
	./mkfs/mkfs.ultifs tunix.img n l src/sys/boot/flashboot.bin w
	./mkfs/mkfs.ultifs image n l src/sys/boot/flashboot.bin i compiled W

c1541_image:
	@echo "# Making c1541 disk image."
	mkdir -p bin
	cp src/sbin/ultiburn/ultiburn bin/
	cp src/sbin/ultidump/ultidump bin/
	cp src/sbin/ultitest/ultitest bin/
	cp src/bin/vi/README.md bin/vi.md
	cp src/bin/vi/vi bin/
	cp src/bin/lisp/README.md bin/lisp.md
	cp src/bin/lisp/lisp bin/
	c1541 -format "tunix,01" d64 tunix.d64 -write bin/ultiburn -write bin/ultidump -write bin/ultitest -write bin/vi -write bin/vi.md -write bin/lisp -write bin/lisp.md

clean:
	@echo "# Cleaning all."
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	$(RM) -rf bin/ image tunix.d64 tunix.img
