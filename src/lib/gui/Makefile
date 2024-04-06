LIBRARY=libgui.a
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

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

%.o: %.c
	$(CC) -O -c $(CFLAGS) -I../.. -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.c=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.c=.o) $(LIBRARY)
