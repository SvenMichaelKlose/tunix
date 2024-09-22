include src/mk/Makefile.build

TAG 			 := $(shell git describe --tags 2>/dev/null)
BRANCH 			 := $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null)
DISTDIR_BASE 	   ?= tunix
DISTDIR 	 	    = $(DISTDIR_BASE)/$(TARGET)/
RELEASE_LISP_FLAGS ?= -DVERBOSE_LOAD=1 -DVERBOSE_DEFINES=1
RELEASE_ZIP_NAME   ?= tunix.$(TAG).zip

ULTIMEM_IMG 		= tunix.img
ULTIMEM_IMG_TRIMMED = tunix.trimmed.img

all: src/include/git-version.h
	$(MAKE) host
	$(MAKE) src
	$(MAKE) mkfs/mkfs.ultifs
	$(MAKE) ultimem_image
#	sbcl --noinform --core bender/bender src/lib/gfx/gencode.lsp

src/include/git-version.h: FORCE
	printf "$(TAG)" >git-version
	printf "(var +v+ \"$(TAG)\")\n" >src/bin/lisp/git-version.lsp
	printf "(var +vb+ \"$(BRANCH)\")\n" >>src/bin/lisp/git-version.lsp
	printf "(out \"TUNIX Lisp (\" +v+ \" \" +vb+ \"" >>src/bin/lisp/git-version.lsp
	printf " on \" +target+ \")\")(terpri)\n" >>src/bin/lisp/git-version.lsp
	mkdir -p src/include
	printf "#define TUNIX_GIT_SHA \"" >src/include/git-version.h
	printf "$(shell git rev-parse HEAD)" >>src/include/git-version.h
	printf "\"\n" >>src/include/git-version.h
	printf "#define TUNIX_GIT_VERSION \"" >>src/include/git-version.h
	cat git-version >>src/include/git-version.h
	printf "\"\n" >>src/include/git-version.h

FORCE:

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
	cp src/bin/lisp/edit-help.md $(DISTDIR)/
ifneq (,$(filter $(TARGET), $(CC65_TARGETS)))
	cp src/bin/lisp/lisp.lbl $(DISTDIR)/
	cp src/bin/lisp/lisp.map $(DISTDIR)/
	cp src/bin/lisp/lisp.dbg $(DISTDIR)/
endif
	cp src/bin/lisp/*.lsp $(DISTDIR)/
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
	$(MAKE) worldclean world TARGET=apple2
	$(MAKE) worldclean world TARGET=apple2enh
	#$(MAKE) worldclean world TARGET=atarixl
	$(MAKE) worldclean world TARGET=c128
	$(MAKE) worldclean world TARGET=c16
	$(MAKE) worldclean world TARGET=c64
	$(MAKE) worldclean world TARGET=pet
	$(MAKE) worldclean world TARGET=plus4
	$(MAKE) worldclean world TARGET=sim6502
	$(MAKE) worldclean world TARGET=unix
	$(MAKE) worldclean world TARGET=vic20

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
	$(MAKE) host COPTFLAGS="-Ofast -flto -march=native" LDFLAGS="-Ofast -flto -march=native"
	$(MAKE) test TARGET=unix
	$(MAKE) allworlds NDEBUG=1 LISP_FLAGS="-DVERBOSE_LOAD -DVERBOSE_DEFINES -DTEST_EVIRONMENT"
	cd src/bin/lisp/doc && ./md2pdf.sh && cd -
	cp src/bin/lisp/doc/manual.pdf tunix/tunix-lisp.pdf
	cp src/bin/lisp/doc/manual.md tunix/tunix-lisp.md
	rm -f $(RELEASE_ZIP_NAME)
	zip -r -o $(RELEASE_ZIP_NAME) tunix
