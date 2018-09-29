SOURCES= \
    file-window.c \
    main.c \
    ultimem.c \
    ultifs.c

ASMSOURCES= \
    launch.asm \
    prgend.asm \
    ../zeropage.asm \
    ../core/mem/clrram.asm \
    ../core/dev/con/charset-4x8.asm

PROGRAM=flashmenu.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) --register-space 19 -O -c $(CFLAGS) -I $(CC65_INCLUDE) -I../lib/ultimem-basics/ -I../lib/gfx/ -I../lib/gui/ -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -m $@.map -Ln $@.lst -o $@ $^ --lib ../lib/ultimem-basics/libultimem-basics.a --lib ../lib/gfx/libgfx.a --lib ../lib/gui/libgui.a --lib $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map desktop.bin ultifs.bin
