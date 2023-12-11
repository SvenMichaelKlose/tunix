PROGRAM=term-test
SOURCES= \
	main.c

include ../../Makefile.rules

%.o: %.c
	$(CC) -c $(CFLAGS) -I../.. -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM).lbl -o $@ $^ ../term/libterm.a ../gfx/libgfx.a ../blit/libblit.a ../ingle/libingle.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl
