SOURCES= \
    linelist.c \
    motion.c

LIBRARY=libtext.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../lineedit/ -I../ingle/ -o $@ $<

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
