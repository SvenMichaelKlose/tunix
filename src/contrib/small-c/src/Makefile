#       Requires System V make
#       @(#)Makefile 1.5 86/05/13
.SUFFIXES:      .o .c .c~ .h .h~
.PRECIOUS:      scclib.a
#       You'll probably want to change these.  These are used by the compilers
#       to figure out where the include files should go.
#	INCDIR will be the look up location for #include <stdio.h> ..
TARGDIR = /home/notebook/Public/work/smallCextended/lib
#INCDIR = "/usr/shared/smallC/src/scc/include/"
INCDIR = "./include/"

INSTFLAGS = -DINCDIR=$(INCDIR)
   CFLAGS = '$(INSTFLAGS)' -ggdb -g

ifdef SystemRoot
   # windows
   CFLAGS = '$(INSTFLAGS)'
else
   ifeq ($(shell uname), Linux)
       CFLAGS = '$(INSTFLAGS)'
   endif
endif

SRC = initials.c data.c error.c expr.c function.c gen.c io.c lex.c main.c preproc.c \
      primary.c stmt.c sym.c while.c struct.c ir.c

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: scc6502
	./scc6502 test/test.c

mkir:
	$(CC) -o mkir $(CFLAGS) mkir.c

ir.h: mkir
	./mkir > ir.h

scc6502: ir.h $(OBJ)
	$(CC) -o scc6502 $(CFLAGS) $(OBJ)

clean:
	rm -f $(OBJ) scc6502 ./test/test.s *~ */*~ mkir mkir.o ir.h

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
