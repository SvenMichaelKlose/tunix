SOURCES= \
    box.asm \
    _bytecodes.asm \
    calcscr.asm \
    clear-screen.asm \
    clip.asm \
    _column-addrs.asm \
    compress-font.asm \
    copy-area.asm \
    get-text-width.asm \
    end.asm frame.asm \
    hline.asm init.asm \
    init-bitmap-mode.asm \
    instructions.asm \
    libgfx-wrapper.asm \
    line.asm \
    masks.asm \
    patterns.asm \
    putchar.asm \
    putchar-fixed-4x4.asm \
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
