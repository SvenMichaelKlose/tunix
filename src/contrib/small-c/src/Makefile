TARGDIR = /home/notebook/Public/work/smallCextended/lib
INCDIR = "./include/"

SCC = scc

INSTFLAGS = -DINCDIR=$(INCDIR)
   CFLAGS = '$(INSTFLAGS)' -ggdb -g

SRC = initials.c data.c error.c expr.c function.c gen.c io.c lex.c main.c preproc.c \
      primary.c stmt.c sym.c while.c struct.c ir.c

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(SCC) test/.passed

test/.passed:
	$(MAKE) -C test all

clean:
	rm -f $(OBJ) $(SCC) mkir mkir.o ir2txt ir2txt.o ir-codes.h
	$(MAKE) -C test clean

ir2txt:
	$(CC) -o ir2txt $(CFLAGS) ir2txt.c

ir.h: mkir
	./mkir > ir-codes.h

mkir:
	$(CC) -o mkir $(CFLAGS) mkir.c

$(SCC): ir.h ir2txt $(OBJ)
	$(CC) -o $(SCC) $(CFLAGS) $(OBJ)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
