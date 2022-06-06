LIBRARY=libultifs.a
SOURCES= \
    ultifs.c
ASMSOURCES= \
    ultifs.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../ultimem/ -o $@ $<

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
