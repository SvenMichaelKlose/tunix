SOURCES= \
    box.asm \
    _bytecodes.asm \
    calcscr.asm \
    clear-screen.asm \
    clip.asm \
    _column-addrs.asm \
    compress-font.asm \
    get-text-width.asm \
    end.asm frame.asm \
    hline.asm init.asm \
    init-bitmap-mode.asm \
    instructions.asm \
    interpreter.asm \
    masks.asm \
    patterns.asm \
    putchar.asm \
    putstring.asm \
    region-stack.asm \
    reset-region.asm \
    start.asm \
    vfill.asm \
    vline.asm \
    vic-settings.inc.asm \
    zeropage.asm

LIBRARY=libgfx.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
