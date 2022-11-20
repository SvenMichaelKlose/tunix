LIBRARY=libultifs.a
ASMSOURCES= \
    ultifs.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

$(LIBRARY): $(ASMSOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(LIBRARY)
