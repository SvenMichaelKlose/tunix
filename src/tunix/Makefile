PROGRAM=tunix.prg
CRUNSHED=tunix
ASMSOURCES= \
	main.asm

include ../Makefile.rules

%.o: %.asm
	$(AS) $(ASFLAGS) -I$(CA65_INCLUDE) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I../lib/ -o $@ $<

all: $(CRUNSHED)

$(CRUNSHED): $(PROGRAM)
	$(EXOMIZER) sfx basic -x3 $(PROGRAM) -t52 -o $(CRUNSHED)

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -o $@ $^ ../lib/tunix/libtunix.a ../lib/ultimem/libultimem.a $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map $(PROGRAM).dbg $(CRUNSHED)
