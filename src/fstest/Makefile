PROGRAM=fstest
SOURCES=main.c
LIBRARIES=$(TUNIX_LIB)/posix/libposix.a $(TUNIX_LIB)/log/liblog.a

include ../Makefile.native

all: $(PROGRAM)
	c1541 -format "fstest,01" d64 test.d64 -write fstest
