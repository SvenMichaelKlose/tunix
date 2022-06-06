PROGRAM=ultifs
SOURCES=kernal-emulation.c main.c ultifs.c
ASMSOURCES=reset.asm wedge-primary.asm wedge-secondary.asm #sjload.asm

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -Ln $(PROGRAM).lbl -o $@ $^ ../lib/ultimem/libultimem.a ../lib/posix/libposix.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map
