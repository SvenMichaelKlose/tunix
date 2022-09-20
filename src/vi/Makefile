PROGRAM=vi
SOURCES= \
	commands.c \
	main.c \
	keyboard.c \
	screen.c

include ../Makefile.rules

%.o: %.c
	$(CC) -c $(CFLAGS) -I.. -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lbl -o $@ $^ ../lib/text/libtext.a ../lib/lineedit/liblineedit.a ../lib/term/libterm.a ../lib/gfx/libgfx.a ../lib/blit/libblit.a ../lib/ingle/libingle.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)
	cp vi vi.prg
	$(EXOMIZER) sfx basic -t 52 vi.prg -o vi
	zip vi.zip README.md vi

clean:
	rm -f $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl vi.zip vi.prg
