SOURCES= \
    box.c \
    button.c \
    error.c \
    event.c \
    frame.c \
    inputline.c \
    layout-ops.c \
    list.c \
    table.c \
    message.c \
    obj.c \
    window.c

LIBRARY=libgui.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../ingle/ -I../gfx/ -I../ultimem/ -o $@ $<

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
