include src/mk/Makefile.build

TAG := $(shell git describe --tags 2>/dev/null)
DISTDIR = tunix/$(TARGET)/
ULTIMEM_IMG = tunix.img
ULTIMEM_IMG_TRIMMED = tunix.trimmed.img

all: src/include/git-version.h host world mkfs/mkfs.ultifs ultimem_image dist
	@echo "# Making all."
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

src/include/git-version.h:
	echo -n $(TAG) >git-version
	echo -n "(var +v+ \"" >src/bin/lisp/git-version.lisp
	cat git-version >>src/bin/lisp/git-version.lisp
	echo "\")" >>src/bin/lisp/git-version.lisp
	echo -n "(out \"TUNIX Lisp (\")(out +v+)(out \"" >>src/bin/lisp/git-version.lisp
	echo -n ")\")(terpri)" >>src/bin/lisp/git-version.lisp
	mkdir -p src/include
	echo -n "#define TUNIX_GIT_VERSION \"" >src/include/git-version.h
	cat git-version >>src/include/git-version.h
	echo "\"" >>src/include/git-version.h

host:
	$(MAKE) -C src host

hosttest:
	$(MAKE) -C src hosttest

hostclean:
	$(MAKE) -C src hostclean

allworlds: host
	$(MAKE) clean all TARGET=pet
	$(MAKE) clean all TARGET=vic20
	$(MAKE) clean all TARGET=c64
	$(MAKE) clean all TARGET=c128
	$(MAKE) clean all TARGET=c16
	$(MAKE) clean all TARGET=plus4
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

ultimem_image:
ifeq ($(TARGET), vic20)
	@echo "# Making UltiMem ROM image."
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG) n l src/sys/boot/flashboot.bin w
	@echo "# Making trimmed UltiMem ROM image."
	./mkfs/mkfs.ultifs $(ULTIMEM_IMG_TRIMMED) n l src/sys/boot/flashboot.bin i compiled W
endif

dist: all
	mkdir -p $(DISTDIR)
	cp src/bin/lisp/lisp $(DISTDIR)/
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

clean:
	$(MAKE) -C src clean
	$(RM) -f git-version src/include/git-version.h
	$(RM) -rf $(DISTDIR)
ifeq ($(TARGET), vic20)
	$(MAKE) -C mkfs clean
	$(RM) -rf $(ULTIMEM_IMG) $(ULTIMEM_IMG_TRIMMED)
endif
