LIBRARY=libkeyboard.a
SOURCES= \
    keyboard.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
