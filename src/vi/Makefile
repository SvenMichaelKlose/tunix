SOURCES= \
	line.c \
	linebuf.c \
	main.c
PROGRAM=vi

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I../lib/term/ -I../lib/ingle -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/term/libterm.a ../lib/gfx/libgfx.a ../lib/blit/libblit.a ../lib/ingle/libingle.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
