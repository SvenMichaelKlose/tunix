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

allworlds: host
	$(MAKE) clean all TARGET=c64
	$(MAKE) clean all TARGET=c128
	$(MAKE) clean all TARGET=c16
	$(MAKE) clean all TARGET=pet
	$(MAKE) clean all TARGET=plus4
	$(MAKE) clean all TARGET=vic20
	$(MAKE) clean all TARGET=unix

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

D64_TUNIX = tunix-lisp.$(TARGET).d64

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
	c1541 -format "tunix,01" d64 $(D64_TUNIX) -write bin/lisp -write bin/lisp.md -write bin/adjoin.lisp -write bin/alist.lisp -write bin/all.lisp -write bin/copy.lisp -write bin/defsetfn.lisp -write bin/do.lisp -write bin/dolist.lisp -write bin/dotimes.lisp -write bin/ensure-list.lisp -write bin/equality.lisp -write bin/every.lisp -write bin/find-if.lisp -write bin/find.lisp -write bin/group.lisp -write bin/intersect.lisp -write bin/let.lisp -write bin/list.lisp -write bin/macroexpand.lisp -write bin/max.lisp -write bin/member-if.lisp -write bin/nthcdr.lisp -write bin/prog1.lisp -write bin/progn.lisp -write bin/quasiquote.lisp -write bin/queue.lisp -write bin/queue-pop.lisp -write bin/remove-if.lisp -write bin/set-difference.lisp -write bin/set-exclusive-or.lisp -write bin/some.lisp -write bin/source.lisp -write bin/stack.lisp -write bin/subseq.lisp -write bin/subseqp.lisp -write bin/union.lisp -write bin/unique.lisp -write bin/unless.lisp -write bin/when.lisp /while.lisp -write bin/with.lisp
endif

clean:
	@echo "# Cleaning for target $(TARGET)."
	$(MAKE) -C src clean
	$(MAKE) -C mkfs clean
	$(RM) -rf bin/
ifneq (,$(TARGET), $(COMMODORE_TARGETS))
	$(RM) -rf $(ULTIMEM_IMG) $(ULTIMEM_IMG_TRIMMED) $(D64_TUNIX)
endif
