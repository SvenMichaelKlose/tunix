PROGRAM = tunix.prg
CRUNSHED = tunix
PROGRAM_init = init.prg
CRUNSHED_init = init
ASMSOURCES = main.asm
CSOURCES = start.c
SOURCES_init = init.c
LD65CFG = vic.cfg

include ../../mk/Makefile.config

ifdef TEST
CFLAGS += -DEARLY_TESTS
endif

%.o: %.asm
	$(AS) $(ASFLAGS) -I$(CA65_INCLUDE) -o $@ $<

%.o: %.c
	$(CC) -c $(CFLAGS) -I$(TUNIX_LIB) -o $@ $<

all: $(CRUNSHED) $(CRUNSHED_init)

$(CRUNSHED): $(PROGRAM)
	$(EXOMIZER) sfx basic $(EXOMIZER_FLAGS) $(PROGRAM) -t52 -o $(CRUNSHED)

$(CRUNSHED_init): $(PROGRAM_init)
	$(EXOMIZER) sfx basic $(EXOMIZER_FLAGS) $(PROGRAM_init) -t52 -o $(CRUNSHED_init)

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(CSOURCES:.c=.o)
	$(LD) $(LDFLAGS) -o $@ $^ $(TUNIX_LIB)/tunix/libtunix.a $(TUNIX_LIB)/ultimem/libultimem.a $(TUNIX_LIB)/zp/libzp.a $(CC65_LIB)/vic20.lib

$(PROGRAM_init): $(SOURCES_init:.c=.o)
	$(LD) -C $(CC65_CFG)/vic20-32k.cfg -Ln $(PROGRAM_init).lbl -m $(PROGRAM_init).map --dbgfile $(PROGRAM_init).dbg -o $@ $^ $(TUNIX_LIB)/tunix/libtunix.a $(CC65_LIB)/vic20.lib

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(CSOURCES:.c=.o) $(PROGRAM) $(PROGRAM).lbl $(PROGRAM).map $(PROGRAM).dbg $(CRUNSHED)
	rm -f $(SOURCES_init:.c=.o) $(PROGRAM_init) $(PROGRAM_init).lbl $(PROGRAM_init).map $(PROGRAM_init).dbg $(CRUNSHED_init)

test:
