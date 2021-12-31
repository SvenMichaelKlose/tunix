SOURCES= \
    basic-starter.c \
    main.c

PROGRAM=basic-starter.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I $(CC65_INCLUDE) -I.. -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -v -C vic20.cfg -m $@.map -Ln $@.lst -o $@ $^ --lib ../lib/gui/libgui.a --lib ../lib/gfx/libgfx.a --lib ../lib/ingle/libingle.a --lib ../lib/ultimem/libultimem.a --lib ../lib/blit/libblit.a --lib $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map desktop.bin desktop-ultifs.bin charset-4x8.o charset-4x8.bin desktop-file-window.bin
