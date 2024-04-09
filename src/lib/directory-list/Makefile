LIBRARY=libdirectorylist.a
SOURCES= \
    main.c

include ../../Makefile.rules

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../../ -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
