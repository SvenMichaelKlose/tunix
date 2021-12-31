SOURCES=

ASMSOURCES= \
    strnlen.asm

LIBRARY=libposix.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -o $@ $<

$(LIBRARY): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o) $(LIBRARY)
