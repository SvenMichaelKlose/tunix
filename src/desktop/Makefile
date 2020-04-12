SOURCES= \
    file-window.c \
    main.c \
    save-state.c \
    ultifs.c \
    wrap-file-window.c \
    wrap-ultifs.c

ASMSOURCES= \
    prgend.asm \
    ../attic/core/mem/clrram.asm

PROGRAM=desktop-lib.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) --register-space 19 -O -Or -Os -Oi -c $(CFLAGS) -I $(CC65_INCLUDE) -I../lib/ingle/ -I../lib/ultimem/ -I../lib/gfx/ -I../lib/gui/ -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -m $@.map -Ln $@.lst -o $@ $^ --lib ../lib/ingle/libingle.a --lib ../lib/ultimem/libultimem.a --lib ../lib/gfx/libgfx.a --lib ../lib/gui/libgui.a --lib $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)
	$(AS) -o charset-4x8.o ../attic/core/dev/con/charset-4x8.asm
	$(LD) -C ../attic/core/dev/con/charset.cfg -o charset-4x8.bin charset-4x8.o

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst $(PROGRAM).map desktop.bin desktop-ultifs.bin charset-4x8.o charset-4x8.bin desktop-file-window.bin
