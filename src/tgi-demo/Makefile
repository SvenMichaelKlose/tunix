SOURCES= \
    main.c

ASMSOURCES= \
    prgend.asm \
    ../zeropage.asm \
    ../attic/core/mem/clrram.asm

PROGRAM=tgi-demo.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) --register-space 19 -O -c $(CFLAGS) -I $(CC65_INCLUDE) -I../lib/gfx/ -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -m $@.map -Ln $@.lst -o $@ $^ --lib $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)
	$(AS) -o charset-4x8.o ../attic/core/dev/con/charset-4x8.asm
	$(LD) -C ../attic/core/dev/con/charset.cfg -o charset-4x8.bin charset-4x8.o

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map desktop.bin ultifs.bin charset-4x8.o charset-4x8.bin
