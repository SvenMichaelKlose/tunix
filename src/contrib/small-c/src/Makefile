.SUFFIXES:      .o .c .c~ .h .h~
.PRECIOUS:      scclib.a

TARGDIR = /home/notebook/Public/work/smallCextended/lib
INCDIR = "./include/"

SCC= scc

INSTFLAGS = -DINCDIR=$(INCDIR)
   CFLAGS = '$(INSTFLAGS)' -ggdb -g

SRC = initials.c data.c error.c expr.c function.c gen.c io.c lex.c main.c preproc.c \
      primary.c stmt.c sym.c while.c struct.c ir.c

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: scc
	./scc test/test.c

mkir:
	$(CC) -o mkir $(CFLAGS) mkir.c

ir.h: mkir
	./mkir > ir.h

scc: ir.h $(OBJ)
	$(CC) -o sc $(CFLAGS) $(OBJ)

clean:
	rm -f $(OBJ) scc ./test/test.s *~ */*~ mkir mkir.o ir.h

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
