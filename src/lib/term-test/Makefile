PROGRAM_xvic=term-test
PROGRAM_sim=term-test-sim
SOURCES= \
	main.c

include ../../Makefile.rules

%.o: %.c
	$(CC) -c $(CFLAGS) -I../.. -o $@ $<

$(PROGRAM_xvic): $(SOURCES:.c=.o)
	$(LD) -C vic.cfg -Ln $(PROGRAM_xvic).lbl -o $@ $^ ../term/libterm.a ../gfx/libgfx.a ../blit/libblit.a ../ingle/libingle.a $(CC65_HOME)/share/cc65/lib/vic20.lib

$(PROGRAM_sim): $(SOURCES:.c=.o)
	$(LD) -t sim6502 -Ln $(PROGRAM_sim).lbl -o $@ $^ ../term/libterm.a ../gfx/libgfx.a ../blit/libblit.a ../ingle/libingle.a $(CC65_HOME)/share/cc65/lib/sim6502.lib

all: $(PROGRAM_xvic) $(PROGRAM_sim)

clean:
	rm -f $(SOURCES:.c=.o) $(PROGRAM_xvic) $(PROGRAM_xvic).lbl $(PROGRAM_sim) $(PROGRAM_sim).lbl
