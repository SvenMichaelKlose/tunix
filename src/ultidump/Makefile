ASMSOURCES=main.asm
PROGRAM=ultidump

include ../Makefile.rules

%.o: %.asm
	$(AS) -I $(CC65_HOME)/share/cc65/asminc/ -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) -C $(CC65_HOME)/share/cc65/cfg/vic20.cfg -Ln $(PROGRAM).lbl -o $@ $^ ../lib/ultimem/libultimem.a $(CC65_HOME)/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl
