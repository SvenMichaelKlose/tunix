TARGDIR = /home/notebook/Public/work/smallCextended/lib
INCDIR = "./include/"

SCC = scc

INSTFLAGS = -DINCDIR=$(INCDIR)
   CFLAGS = '$(INSTFLAGS)' -ggdb -g -Wall -Werror -Wno-error=parentheses

SRC = initials.c data.c error.c expr.c function.c gen.c io.c lex.c main.c preproc.c \
      primary.c stmt.c sym.c while.c struct.c ir.c

OBJ = $(addsuffix .o, $(basename $(SRC)))
TOK = $(addsuffix .tok, $(basename $(SRC)))

all: $(SCC) test/.passed

test/.passed:
	$(MAKE) -C test all

clean:
	rm -f $(TOK) $(OBJ) $(SCC) mkir mkir.o ir2txt ir2txt.o ir-codes.h ctok ctok.o mktok mtok.o token.h
	$(MAKE) -C test clean

$(SCC): ir-codes.h ir2txt token.h ctok $(OBJ) $(TOK)
	$(CC) -o $(SCC) $(CFLAGS) $(OBJ)

ir2txt: ir2txt.o

token.h: mktok
	./mktok > $@

ir-codes.h: mkir
	./mkir > $@

mkir: mkir.o

ctok: ctok.o

mktok: mktok.o

%.tok : %.c
	./ctok $< $@

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
