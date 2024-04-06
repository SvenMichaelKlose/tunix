PROGRAM=ultifs.prg
SOURCES= \
	alloc.c \
	ipc.c \
	kernal.c \
	main.c \
	test.c \
	ultifs.c
ASMSOURCES= \
	reset.asm \
	dispatcher.asm \
	zeropage.asm \
	wedge-secondary.asm

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -Oi -DTEST -o $@ -I../ $<

all: $(PROGRAM)
	$(EXOMIZER) sfx basic -t 52 ultifs.prg -o ultifs

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -o $@ $^ ../lib/ultimem/libultimem.a ../lib/posix/libposix.a ../lib/log/liblog.a $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map $(PROGRAM).dbg ultifs
