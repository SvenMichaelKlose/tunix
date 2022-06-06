SOURCES= \
    main.asm \
    keyboard.asm \
    putchar-fixed-4x4.asm

LIBRARY=libterm.a

include ../../Makefile.rules

%.o: %.asm
	$(AS) $(ASFLAGS) -o $@ $<

$(LIBRARY): $(SOURCES:.asm=.o)
	$(AR) a $@ $^

all: $(LIBRARY)

clean:
	rm -f $(SOURCES:.asm=.o) $(LIBRARY)
