SOURCES=kernal-emulation.c ultifs.c
ASMSOURCES=zeropage.asm gap.asm main.asm wedge-primary.asm wedge-secondary.asm
PROGRAM=ultifs-wedge

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../lib/ultimem/ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
