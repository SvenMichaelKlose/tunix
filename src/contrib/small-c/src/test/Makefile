TARGETMACROS = ../6502/ca65-macros.asm

CC = ../scc
IR2TXT = ../ir2txt
CA65 = ca65
LD65 = ld65

SRC = \
	test-empty-fun.c

PRGS = $(addsuffix .prg, $(basename $(SRC)))

all: $(PRGS)
	touch .passed

.PRECIOUS: %.o %.ir %.irt %.asm %.o2

clean:
	rm -f .passed
	rm -f $(addsuffix .ir, $(basename $(SRC)))
	rm -f $(addsuffix .irt, $(basename $(SRC)))
	rm -f $(addsuffix .asm, $(basename $(SRC)))
	rm -f $(addsuffix .o, $(basename $(SRC)))
	rm -f $(addsuffix .o2, $(basename $(SRC)))
	rm -f $(addsuffix .prg, $(basename $(SRC)))

%.prg : %.o2
	$(LD65) -C ld65-test.cfg -o $@ $<
	cmp expected/$@ $@ || exit

%.o2 : %.asm
	$(CA65) -v -o $@ $<

%.asm : %.irt
	echo ".include \"$(TARGETMACROS)\"" > $@
	cat $< >> $@

%.irt : %.o
	$(IR2TXT) < $< | tee $@

%.o : %.c
	./$(CC) $<
	hexdump -Cv $@
