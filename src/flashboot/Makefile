ASMSOURCES=alloc.asm api.asm copy-bank.asm ealloc.asm launch.asm main.asm process.asm save-state.asm rom-info.asm prgend.asm ultifs-dev.asm
PROGRAM=flashboot.bin

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o) $(ASMSOURCES:.asm=.o)
	$(LD) --config rom.cfg -m $(PROGRAM).map -Ln $(PROGRAM).lst -o $@ $^ ../lib/ingle/libingle.a ../lib/ultifs/libultifs.a ../lib/ultimem/libultimem.a ../lib/blit/libblit.a ../lib/zp/libzp.a

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).map $(PROGRAM).lst ultifs.bin desktop.bin
