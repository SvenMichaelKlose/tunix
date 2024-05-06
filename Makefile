all: world mkfs ultimem_image c1541_image
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

world:
	@echo "# Making world."
	$(MAKE) -C src all

mkfs:
	@echo "# Making host mkfs."
	$(MAKE) -C mkfs all

ultimem_image:
	@echo "# Making UltiMem ROM image."
	./mkfs/mkfs.ultifs ingle.img n l src/flashboot/flashboot.bin w
	./mkfs/mkfs.ultifs image n l src/flashboot/flashboot.bin i compiled W

c1541_image:
	@echo "# Making c1541 disk image."
	mkdir -p bin
	cp src/fstest/fstest bin/
	cp src/tunix/tunix bin/
	cp src/tunix/init bin/
	cp src/cbm-console/cbm-console bin/
	cp src/ultiburn/ultiburn bin/
	cp src/ultidump/ultidump bin/
	cp src/ultitest/ultitest bin/
	cp src/vi/README.md bin/vi.md
	cp src/vi/vi bin/
	c1541 -format "ingle,01" d64 ingle.d64 -write bin/tunix -write bin/init -write bin/cbm-console -write bin/ultiburn -write bin/ultidump -write bin/ultitest -write bin/vi

clean:
	@echo "# Cleaning all."
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	$(RM) -rf bin/ image ingle.d64 ingle.img
