SOURCES= \
    printstr.asm

LIBRARY=libkernal.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
