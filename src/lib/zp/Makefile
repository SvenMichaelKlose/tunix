LIBRARY=libzp.a
SOURCES= \
    dword.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
