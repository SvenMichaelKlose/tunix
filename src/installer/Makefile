ASMSOURCES=zeropage.asm main.asm
PROGRAM=ingle

include ../Makefile.rules

%.o: %.asm
	$(AS) -I /usr/local/share/cc65/asminc/ -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -t vic20 -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
