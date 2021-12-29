ifndef TV
$(error Please set TV=PAL or TV=NTSC)
endif

ASMSOURCES= rom.asm
PROGRAM=rom

include ../Makefile.rules

%.o: %.asm
	$(AS) -D$(TV)=1 -o $@ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o)
	$(LD) -C ld.cfg -Ln $(PROGRAM).lbl -o $@ $^

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lbl
