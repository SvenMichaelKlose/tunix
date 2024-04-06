LIBRARY=liblog.a
SOURCES= \
    log.c

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../../ -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
