include src/mk/Makefile.build

TAG 			 := $(shell git describe --tags 2>/dev/null)
DISTDIR_BASE 	   ?= tunix
DISTDIR 	 	    = $(DISTDIR_BASE)/$(TARGET)/
RELEASE_LISP_FLAGS ?= -DVERBOSE_LOAD=1 -DVERBOSE_DEFINES=1
RELEASE_ZIP_NAME   ?= tunix.$(TAG).zip

ULTIMEM_IMG 		= tunix.img
ULTIMEM_IMG_TRIMMED = tunix.trimmed.img

all: src/include/git-version.h #host src mkfs/mkfs.ultifs ultimem_image
	$(MAKE) host
	$(MAKE) src
	$(MAKE) mkfs/mkfs.ultifs
	$(MAKE) ultimem_image
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lisp

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
	$(MAKE) -C src clean
ifeq ($(TARGET), vic20)
	$(MAKE) -C mkfs clean
	rm -rf $(ULTIMEM_IMG) $(ULTIMEM_IMG_TRIMMED)
endif
	rm -rf $(DISTDIR)

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

test: all
	$(MAKE) -C src test
	./scripts/test-unix.sh

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

clean:
	for i in $(VALID_TARGETS); do \
    	$(MAKE) -C src clean TARGET=$$i; \
    done
	$(MAKE) -C mkfs clean
	rm -rf $(DISTDIR_BASE)
	rm -f git-version src/include/git-version.h
	@if [ -d src/include ]; then rmdir src/include; fi

.NOTPARALLEL: release
release:
	$(MAKE) clean hostclean
	git submodule update --init --recursive
	@git status --porcelain | grep "^??" > /dev/null && { \
		printf "There are untracked files in the repository which are also not ignored:\n"; \
		git status --porcelain | grep "^??"; \
		read -p "Do you want to continue anyway? [y/N] " answer; \
		if [ "$$answer" != "y" ]; then \
			printf "User aborted.\n"; \
			exit 1; \
		fi \
	} || { \
		printf "No untracked files, proceeding with release.\n"; \
	}
	@echo "Running the release process for '$(RELEASE_ZIP_NAME)'..."
	$(MAKE) host
	$(MAKE) test TARGET=unix
	for target in c128 c16 pet vic20; do \
   		$(MAKE) world TARGET=$$target NDEBUG=1 LISP_FLAGS="$(RELEASE_LISP_FLAGS) -DCOMPRESSED_CONS"; \
	done
	for target in c64 plus4 unix; do \
   		$(MAKE) world TARGET=$$target NDEBUG=1 LISP_FLAGS="$(RELEASE_LISP_FLAGS)"; \
	done
	cd src/bin/lisp/doc && ./md2pdf.sh && cd -
	cp src/bin/lisp/doc/manual.pdf tunix/tunix-lisp.pdf
	cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
	rm -f $(RELEASE_ZIP_NAME)
	zip -r -o $(RELEASE_ZIP_NAME) tunix
