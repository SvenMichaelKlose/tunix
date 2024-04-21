.SUFFIXES:      .o .c .c~ .h .h~
.PRECIOUS:      scclib.a

TARGDIR = /home/notebook/Public/work/smallCextended/lib
INCDIR = "./include/"

SCC = scc

INSTFLAGS = -DINCDIR=$(INCDIR)
   CFLAGS = '$(INSTFLAGS)' -ggdb -g

SRC = initials.c data.c error.c expr.c function.c gen.c io.c lex.c main.c preproc.c \
      primary.c stmt.c sym.c while.c struct.c ir.c

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(SCC)
	./$(SCC) test/test.c
	./ir2txt < test/test.o

ir2txt:
	$(CC) -o ir2txt $(CFLAGS) ir2txt.c

mkir:
	$(CC) -o mkir $(CFLAGS) mkir.c

ir.h: mkir
	./mkir > ir.h

$(SCC): ir.h ir2txt $(OBJ)
	$(CC) -o $(SCC) $(CFLAGS) $(OBJ)

clean:
	rm -f $(OBJ) $(SCC) ./test/test.s *~ */*~ mkir mkir.o ir2txt ir2txt.o ir.h

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
