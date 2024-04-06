LIBRARY=libtunix.a
ASMSOURCES= \
    main.asm \
    c-wrappers.asm

include ../../Makefile.rules

%.o: %.asm
	$(AS) -g -o $@ $<

$(LIBRARY): $(ASMSOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(ASMSOURCES:.asm=.o) $(CSOURCES:.asm=.o) $(LIBRARY)
