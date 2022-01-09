ASMSOURCES=main.asm
PROGRAM=sjload

include ../Makefile.rules

%.o: %.asm
	$(AS) -o $@ $<

$(PROGRAM): $(ASMSOURCES:.asm=.o) $(SOURCES:.c=.o)
	$(LD) -t none -Ln $(PROGRAM).lst -o $@ $^ #/usr/local/share/cc65/lib/vic20.lib

all: $(PROGRAM)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(PROGRAM) $(PROGRAM).lst
