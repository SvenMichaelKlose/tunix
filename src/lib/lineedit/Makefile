LIBRARY=liblineedit.a
SOURCES= \
    linebuf.c \
    main.c

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../../ -o $@ $<

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
