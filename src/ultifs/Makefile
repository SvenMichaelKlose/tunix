SOURCES=kernal-emulation.c main.c ultifs.c
ASMSOURCES=reset.asm wedge-primary.asm wedge-secondary.asm #sjload.asm
PROGRAM=ultifs

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ultimem/libultimem.a ../lib/posix/libposix.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map
