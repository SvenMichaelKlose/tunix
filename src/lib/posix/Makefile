LIBRARY=libposix.a
SOURCES=
ASMSOURCES= \
    strnlen.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o) $(LIBRARY)
