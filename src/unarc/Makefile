PROGRAM=unarc.prg
SOURCES=unarc.c
ASMSOURCES=

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ -I../ $<

all: $(PROGRAM)

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C $(CC65_HOME)/share/cc65/cfg/vic20-32k.cfg -o $@ $^ $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).dbg $(PROGRAM).lbl $(PROGRAM).map
