ASMSOURCES= rom.asm
PROGRAM=rom

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o)
	$(LD) -C vic20.cfg -Ln $(PROGRAM).lbl -o $@ $^

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lbl
