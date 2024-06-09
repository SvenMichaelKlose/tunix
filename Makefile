include src/mk/Makefile.build

all: world mkfs/mkfs.ultifs ultimem_image c1541_image
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

allworlds:
	make clean all TARGET=c64
	make clean all TARGET=c128
	make clean all TARGET=c16
	make clean all TARGET=pet
	make clean all TARGET=plus4
	make clean all TARGET=unix
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

ULTIMEM_IMG = tunix.img
ULTIMEM_IMG_TRIMMED = tunix.trimmed.img

ultimem_image:
ifeq ($(TARGET), vic20)
	@echo "# Making UltiMem ROM image."
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG) n l src/sys/boot/flashboot.bin w
	@echo "# Making trimmed UltiMem ROM image."
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG_TRIMMED) n l src/sys/boot/flashboot.bin i compiled W
endif

D64_TUNIX_TOOLS = tunix-tools.$(TARGET).d64

c1541_image:
	@echo "# Making c1541 disk image."
	mkdir -p bin
	cp src/bin/lisp/README.md bin/lisp.md
	cp src/bin/lisp/lisp bin/
	cp src/bin/lisp/env.lisp bin/
	cp src/bin/lisp/test.lisp bin/
	cp src/bin/lisp/list.lisp bin/
	cp src/bin/lisp/macro.lisp bin/
ifeq ($(TARGET), vic20)
	cp src/sbin/ultiburn/ultiburn bin/
	cp src/sbin/ultidump/ultidump bin/
	cp src/sbin/ultitest/ultitest bin/
	cp src/bin/vi/README.md bin/vi.md
	cp src/bin/vi/vi bin/
endif
ifneq (,$(TARGET), $(COMMODORE_TARGETS))
	c1541 -format "tunix,01" d64 $(D64_TUNIX_TOOLS) -write bin/lisp -write bin/env.lisp -write bin/test.lisp -write bin/macro.lisp -write bin/lisp.md #-write bin/ultiburn -write bin/ultidump -write bin/ultitest -write bin/vi -write bin/vi.md
else
	c1541 -format "tunix,01" d64 $(D64_TUNIX_TOOLS) -write bin/lisp -write bin/env.lisp -write bin/lisp.md
endif

clean:
	@echo "# Cleaning for target $(TARGET)."
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	$(RM) -rf bin/
ifneq (,$(TARGET), $(COMMODORE_TARGETS))
	$(RM) -rf $(ULTIMEM_IMG) $(ULTIMEM_IMG_TRIMMED) $(D64_TUNIX_TOOLS)
endif
