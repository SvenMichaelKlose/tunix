include src/mk/Makefile.build

all: host world mkfs/mkfs.ultifs ultimem_image c1541_image
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

host:
	$(MAKE) -C src host

hosttest:
	$(MAKE) -C src hosttest

hostclean:
	$(MAKE) -C src hostclean

allworlds:
	$(MAKE) clean all TARGET=c64
	$(MAKE) clean all TARGET=c128
	$(MAKE) clean all TARGET=c16
	$(MAKE) clean all TARGET=pet
	$(MAKE) clean all TARGET=plus4
	$(MAKE) clean all TARGET=unix
	$(MAKE) clean all TARGET=vic20

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
	cp src/bin/lisp/*.lisp bin/
ifeq ($(TARGET), vic20)
	cp src/sbin/ultiburn/ultiburn bin/
	cp src/sbin/ultidump/ultidump bin/
	cp src/sbin/ultitest/ultitest bin/
	cp src/bin/vi/README.md bin/vi.md
	cp src/bin/vi/vi bin/
endif
ifneq (,$(TARGET), $(COMMODORE_TARGETS))
	c1541 -format "tunix,01" d64 $(D64_TUNIX_TOOLS) -write bin/lisp -write bin/lisp.md -write bin/alist.lisp -write bin/dolist.lisp -write bin/env.lisp -write bin/equality.lisp -write bin/let.lisp -write bin/list.lisp -write bin/macroexpand.lisp -write bin/prog1.lisp -write bin/progn.lisp -write bin/quasiquote.lisp -write bin/set.lisp -write bin/smoke-test.lisp -write bin/stack.lisp -write bin/max.lisp -write bin/nthcdr.lisp -write bin/subseq.lisp -write bin/queue.lisp -write bin/group.lisp -write bin/test.lisp -write bin/when.lisp -write bin/unless.lisp -write bin/while.lisp -write bin/with.lisp #-write bin/ultiburn -write bin/ultidump -write bin/ultitest -write bin/vi -write bin/vi.md
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
