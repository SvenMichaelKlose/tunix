SOURCES_asm= \
    calc-bank.asm \
    c-copy.asm \
    commands.asm \
    copy-rom2ram.asm \
    readwrite.asm \
    unhide.asm

SOURCES= \
    map.c

LIBRARY=libultimem.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../ingle/ -o $@ $<

$(LIBRARY): $(SOURCES_asm:.asm=.o) $(SOURCES:.c=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES_asm:.asm=.o) $(LIBRARY)
