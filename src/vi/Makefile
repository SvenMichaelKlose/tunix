SOURCES= \
	commands.c \
	main.c \
	keyboard.c \
	screen.c
PROGRAM=vi

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I.. -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/text/libtext.a ../lib/lineedit/liblineedit.a ../lib/term/libterm.a ../lib/gfx/libgfx.a ../lib/blit/libblit.a ../lib/ingle/libingle.a /usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst
