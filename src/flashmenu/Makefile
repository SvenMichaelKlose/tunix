SOURCES= \
    bank-allocator.c \
    basic-starter.c \
    file-window.c \
    main.c \
    ultifs.c \
    ultimem.c

ASMSOURCES= \
    cbm.asm \
    ../zeropage.asm \
    ../core/mem/clrram.asm \
    ../core/dev/con/charset-4x8.asm

PROGRAM=flashmenu.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../lib/ultimem-basics/ -I../lib/gfx/ -I../lib/gui/ -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -t vic20g -m $@.map -Ln $@.lst -o $@ $^ --lib ../lib/ultimem-basics/libultimem-basics.a --lib ../lib/gfx/libgfx.a --lib ../lib/gui/libgui.a --lib $(CC65_HOME)/share/cc65/lib/vic20g.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map
