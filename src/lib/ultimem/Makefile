SOURCES_asm= \
    burn.asm \
    bank-offset.asm \
    c-copy.asm \
    commands.asm \
    copy.asm \
    detect.asm \
    erase.asm \
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
	rm -f $(SOURCES:.c=.o) $(SOURCES_asm:.asm=.o) $(LIBRARY)
