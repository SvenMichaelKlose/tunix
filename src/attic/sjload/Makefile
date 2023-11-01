PROGRAM=sjload
ASMSOURCES=main.asm

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) -C $(CC65_HOME)/share/cc65/cfg/none.cfg -Ln $(PROGRAM).lbl -o $@ $^

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lbl
