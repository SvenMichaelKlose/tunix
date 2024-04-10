PROGRAM=cbm-console.prg
CRUNSHED=cbm-console
SOURCES=main.c
ASMSOURCES=interrupt-handler.asm

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c -I.. $(CFLAGS) -o $@ -I../ $<

all: $(CRUNSHED)

$(CRUNSHED): $(PROGRAM)
	$(EXOMIZER) sfx basic -x2 -t 52 $(PROGRAM) -o $(CRUNSHED)

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -o $@ $^ ../lib/tunix/libtunix.a ../lib/blit/libblit.a $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).dbg $(PROGRAM).lbl $(PROGRAM).map $(CRUNSHED)
