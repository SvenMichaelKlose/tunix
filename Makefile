include src/mk/Makefile.build

TAG := $(shell git describe --tags 2>/dev/null)
DISTDIR = tunix/$(TARGET)/
ULTIMEM_IMG = tunix.img
ULTIMEM_IMG_TRIMMED = tunix.trimmed.img

all: src/include/git-version.h host src mkfs/mkfs.ultifs ultimem_image
	@printf "# Making all.\n"
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

clean:
	$(MAKE) -C src clean
	$(RM) -f git-version src/include/git-version.h
ifeq ($(TARGET), vic20)
	$(MAKE) -C mkfs clean
	$(RM) -rf $(ULTIMEM_IMG) $(ULTIMEM_IMG_TRIMMED)
endif

src/include/git-version.h:
	printf "$(TAG)" >git-version
	printf "(var +v+ \"" >src/bin/lisp/git-version.lisp
	cat git-version >>src/bin/lisp/git-version.lisp
	printf "\")\n" >>src/bin/lisp/git-version.lisp
	printf "(out \"TUNIX Lisp (\")(out +v+)(out \"" >>src/bin/lisp/git-version.lisp
	printf ")\")(terpri)\n" >>src/bin/lisp/git-version.lisp
	mkdir -p src/include
	printf "#define TUNIX_GIT_SHA \"" >src/include/git-version.h
	printf "$(shell git rev-parse HEAD)" >>src/include/git-version.h
	printf "\"\n" >>src/include/git-version.h
	printf "#define TUNIX_GIT_VERSION \"" >>src/include/git-version.h
	cat git-version >>src/include/git-version.h
	printf "\"\n" >>src/include/git-version.h

host:
	cp kgetin.s src/contrib/cc65/libsrc/plus4/
	$(MAKE) -C src host

hosttest:
	$(MAKE) -C src hosttest

hostclean:
	$(MAKE) -C src hostclean

src: host
	$(MAKE) -C src all

world: all
	mkdir -p $(DISTDIR)
	rm -f $(DISTDIR)/image
	cp src/bin/lisp/lisp $(DISTDIR)/
	cp src/bin/lisp/test-read.bin $(DISTDIR)/
ifneq (,$(filter $(TARGET), $(CC65_TARGETS)))
	cp src/bin/lisp/lisp.lbl $(DISTDIR)/
	cp src/bin/lisp/lisp.map $(DISTDIR)/
	cp src/bin/lisp/lisp.dbg $(DISTDIR)/
endif
	cp src/bin/lisp/*.lisp $(DISTDIR)/
ifeq ($(TARGET), vic20)
	cp src/sbin/ultiburn/ultiburn $(DISTDIR)/
	cp src/sbin/ultidump/ultidump $(DISTDIR)/
	cp src/sbin/ultitest/ultitest $(DISTDIR)/
	cp src/bin/vi/README.md $(DISTDIR)/vi.md
	cp src/bin/vi/vi $(DISTDIR)/
endif

worldclean:
	$(RM) -rf $(DISTDIR)

allworlds:
	$(MAKE) host
	$(MAKE) clean world TARGET=c128
	$(MAKE) clean world TARGET=c16
	$(MAKE) clean world TARGET=c64
	$(MAKE) clean world TARGET=pet
	$(MAKE) clean world TARGET=plus4
	$(MAKE) clean world TARGET=sim6502
	$(MAKE) clean world TARGET=unix
	$(MAKE) clean world TARGET=vic20

test:
	$(MAKE) -C src test

mkfs/mkfs.ultifs:
ifeq ($(TARGET), vic20)
	@printf "# Making host mkfs.\n"
	$(MAKE) -C mkfs all
endif

ultimem_image:
ifeq ($(TARGET), vic20)
	@printf "# Making UltiMem ROM image.\n"
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG) n l src/sys/boot/flashboot.bin w
	@printf "# Making trimmed UltiMem ROM image.\n"
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG_TRIMMED) n l src/sys/boot/flashboot.bin i compiled W
endif
