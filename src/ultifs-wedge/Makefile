SOURCES=ultifs.c
ASMSOURCES=zeropage.asm main.asm
PROGRAM=ultifs-wedge

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../lib/ultimem/ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
