PROGRAM=fstest.prg
SOURCES=main.c
ASMSOURCES=

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -o $@ $^ ../lib/posix/libposix.a ../lib/log/liblog.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)
	$(EXOMIZER) sfx basic -t 52 fstest.prg -o fstest
	c1541 -format "fstest,01" d64 test.d64 -write fstest

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).dbg $(PROGRAM).lbl $(PROGRAM).map fstest
