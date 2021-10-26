SOURCES=kernal-emulation.c main.c ultifs.c
ASMSOURCES=zeropage.asm gap.asm wedge-primary.asm wedge-secondary.asm
PROGRAM=ultifs

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../lib/ultimem/ -I../lib/ingle/ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
