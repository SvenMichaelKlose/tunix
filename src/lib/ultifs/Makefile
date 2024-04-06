LIBRARY=libultifs.a
ASMSOURCES= \
    ultifs.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

all: $(LIBRARY)

$(LIBRARY): $(ASMSOURCES:.asm=.o)
	$(AR) a $@ $^

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(LIBRARY)
