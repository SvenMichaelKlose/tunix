PROGRAM=tunix.prg
CRUNSHED=tunix
PROGRAM_init=init.prg
CRUNSHED_init=init
ASMSOURCES= \
	main.asm
SOURCES_init= \
	init.c

include ../Makefile.rules

%.o: %.asm
	$(AS) $(ASFLAGS) -I$(CA65_INCLUDE) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I.. -o $@ $<

all: $(CRUNSHED) $(CRUNSHED_init)

$(CRUNSHED): $(PROGRAM)
	$(EXOMIZER) sfx basic $(EXOMIZER_FLAGS) $(PROGRAM) -t52 -o $(CRUNSHED)

$(CRUNSHED_init): $(PROGRAM_init)
	$(EXOMIZER) sfx basic $(EXOMIZER_FLAGS) $(PROGRAM_init) -t52 -o $(CRUNSHED_init)

$(PROGRAM): $(ASMSOURCES:.asm=.o)
	$(LD) $(LDFLAGS) -C vic.cfg -o $@ $^ ../lib/tunix/libtunix.a ../lib/ultimem/libultimem.a ../lib/zp/libzp.a $(CC65_HOME)/share/cc65/lib/vic20.lib

$(PROGRAM_init): $(SOURCES_init:.c=.o)
	$(LD) -Ln $(PROGRAM_init).lbl -m $(PROGRAM_init).map --dbgfile $(PROGRAM_init).dbg -C $(CC65_HOME)/share/cc65/cfg/vic20-32k.cfg -o $@ $^ ../lib/tunix/libtunix.a $(CC65_HOME)/share/cc65/lib/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map $(PROGRAM).dbg $(CRUNSHED)
	rm -f $(SOURCES_init:.c=.o) $(PROGRAM_init) $(PROGRAM_init).lbl $(PROGRAM_init).map $(PROGRAM_init).dbg $(CRUNSHED_init)
