SOURCES= \
    calc-bank.asm \
    commands.asm \
    copy-rom2ram.asm \
    readwrite.asm \
    unhide.asm

LIBRARY=libultimem.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
