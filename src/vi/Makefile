PROGRAM=vi.prg
CRUNSHED=vi
ARCHIVE=vi.zip
ARCHIVE_FILES=README.md $(CRUNSHED)
SOURCES= \
	commands.c \
	main.c \
	keyboard.c \
	screen.c

include ../Makefile.rules

%.o: %.c
	$(CC) -c $(CFLAGS) -I.. -o $@ $<

all: $(ARCHIVE)

$(ARCHIVE): $(CRUNSHED)
	zip $(ARCHIVE) $(ARCHIVE_FILES)

$(CRUNSHED): $(PROGRAM)
	$(EXOMIZER) sfx basic -t 52 $(PROGRAM) -o $(CRUNSHED)

$(PROGRAM): $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lbl -o $@ $^ \
		../lib/text/libtext.a ../lib/lineedit/liblineedit.a \
		../lib/term/libterm.a ../lib/gfx/libgfx.a \
		../lib/blit/libblit.a ../lib/ingle/libingle.a \
		$(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM) $(CRUNSHED) $(ARCHIVE)
