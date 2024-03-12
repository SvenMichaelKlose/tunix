PROGRAM=tunix
ASMSOURCES= \
	main.asm
#SOURCES= \
#	syscalls.c \
#	main.c

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I../lib/ -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lbl -o $@ $^ ../lib/term/libterm.a ../lib/gfx/libgfx.a ../lib/blit/libblit.a ../lib/ingle/libingle.a ../lib/ultimem/libultimem.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl
