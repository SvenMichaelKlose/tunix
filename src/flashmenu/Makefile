SOURCES= \
    bank-allocator.c \
    basic-starter.c \
    box.c \
    button.c \
    error.c \
    event.c \
    frame.c \
    file-window.c \
    layout-ops.c \
    list.c \
    scroll.c \
    table.c \
    main.c \
    message.c \
    obj.c \
    ultimem.c \
    window.c

ASMSOURCES= \
    cbm.asm \
    libgfx-wrapper.asm \
    ultimem-basics.asm \
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
