PROGRAM=flashboot.bin
ASMSOURCES=rom-info.asm start.asm
#ASMSOURCES=alloc.asm api.asm copy-bank.asm launch.asm main.asm save-state.asm rom-info.asm prgend.asm
SOURCES=main.c

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c -I.. $(CFLAGS) -o $@ $<

all: $(PROGRAM)

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) --config rom.cfg -m $(PROGRAM).map -Ln $(PROGRAM).lbl -o $@ $^ ../lib/ultimem/libultimem.a $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).map $(PROGRAM).lbl ultifs.bin desktop.bin
