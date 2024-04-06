PROGRAM=rom
ASMSOURCES= rom.asm

ifndef TV
$(error Please set TV=PAL or TV=NTSC)
endif

include ../Makefile.rules

%.o: %.asm
	$(AS) -D$(TV)=1 -o $@ $<

all: $(PROGRAM)

$(PROGRAM): $(ASMSOURCES:.asm=.o)
	$(LD) -C ld.cfg -Ln $(PROGRAM).lbl -o $@ $^

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lbl
