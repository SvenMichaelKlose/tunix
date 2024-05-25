include src/config

all: world mkfs/mkfs.ultifs ultimem_image c1541_image
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

allworlds:
	make clean all TARGET=c64
	make clean all TARGET=c128
	make clean all TARGET=pet
	make clean all TARGET=plus4
	make clean all TARGET=vic20

world:
	@echo "# Making $(TARGET) world."
	$(MAKE) -C src all

test:
	$(MAKE) -C src test

mkfs/mkfs.ultifs:
ifeq ($(TARGET), vic20)
	@echo "# Making host mkfs."
	$(MAKE) -C mkfs all
endif

ultimem_image:
ifeq ($(TARGET), vic20)
	@echo "# Making UltiMem ROM image."
	./mkfs/mkfs.ultifs tunix.img n l src/sys/boot/flashboot.bin w
	./mkfs/mkfs.ultifs image n l src/sys/boot/flashboot.bin i compiled W
endif

c1541_image:
	@echo "# Making c1541 disk image."
	mkdir -p bin
	cp src/bin/lisp/README.md bin/lisp.md
	cp src/bin/lisp/lisp bin/
	cp src/bin/lisp/env.lisp bin/
ifeq ($(TARGET), vic20)
	cp src/sbin/ultiburn/ultiburn bin/
	cp src/sbin/ultidump/ultidump bin/
	cp src/sbin/ultitest/ultitest bin/
	cp src/bin/vi/README.md bin/vi.md
	cp src/bin/vi/vi bin/
endif
ifeq ($(filter $(TARGET), atarixl vic20),)
	c1541 -format "tunix,01" d64 tunix-tools.$(TARGET).d64 -write bin/lisp -write bin/env.lisp -write bin/lisp.md
endif
ifeq ($(TARGET), vic20)
	c1541 -format "tunix,01" d64 tunix-tools.$(TARGET).d64 -write bin/lisp -write bin/env.lisp -write bin/lisp.md -write bin/ultiburn -write bin/ultidump -write bin/ultitest -write bin/vi -write bin/vi.md
endif

clean:
	@echo "# Cleaning all."
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	$(RM) -rf bin/ image tunix.d64 tunix.img tunix-tools.*.zip
