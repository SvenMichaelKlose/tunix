PROGRAM=desktop-lib.bin
SOURCES= \
    desktop.c \
    file-window.c \
    main.c \
    save-state.c \
    wrap-file-window.c
ASMSOURCES= \
    prgend.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) --register-space 19 -O -c $(CFLAGS) -I$(CC65_INCLUDE) -I$(TUNIX_LIB) -o $@ $<

all: $(PROGRAM)
	$(AS) -o charset-4x8.o charset-4x8-light.asm
	$(LD) -C charset.cfg -o charset-4x8.bin charset-4x8.o

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -m $@.map -Ln $@.lbl -o $@ $^ $(TUNIX_LIB)/gui/libgui.a $(TUNIX_LIB)/gfx/libgfx.a $(TUNIX_LIB)/ultimem/libultimem.a $(TUNIX_LIB)/ingle/libingle.a $(TUNIX_LIB)/blit/libblit.a --lib $(CC65_LIB)/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map desktop.bin charset-4x8.o charset-4x8.bin desktop-file-window.bin
