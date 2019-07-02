ASMSOURCES=main.asm launch.asm save-state.asm rom-info.asm prgend.asm ../attic/core/mem/moveram.asm ../attic/core/mem/clrram.asm
PROGRAM=flashboot.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) --config rom.cfg -Ln $(PROGRAM).lst -o $@ $^ ../lib/ingle/libingle.a ../lib/ultifs/libultifs.a ../lib/ultimem/libultimem.a

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lst ultifs.bin desktop.bin
