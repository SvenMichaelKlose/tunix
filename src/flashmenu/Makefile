SOURCES= \
    button.c \
    error.c \
    layout-ops.c \
    list.c \
    main.c \
    obj.c \
    window.c

ASMSOURCES= \
    libgfx-wrapper.asm \
    ../zeropage.asm \
    ../core/mem/clrram.asm \
    ../core/dev/con/charset-4x8.asm

PROGRAM=flashmenu.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -t vic20g -m $@.map -Ln $@.lst -o $@ $^ --lib ../lib/gfx/libgfx.a --lib $(CC65_HOME)/share/cc65/lib/vic20g.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map
