ASMSOURCES= main.asm ../attic/core/mem/clrram.asm
PROGRAM=play

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a ../lib/ingle/libingle.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
