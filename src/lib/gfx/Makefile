SOURCES=box.asm _bytecodes.asm calcscr.asm clear-screen.asm clip.asm column-addrs.asm compress-font.asm end.asm frame.asm hline.asm init.asm init-bitmap-mode.asm masks.asm patterns.asm putchar.asm putstring.asm reset-region.asm start.asm vfill.asm vic-settings.inc.asm vline.asm zeropage.asm
LIBRARY=libgfx.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
