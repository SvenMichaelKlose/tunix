PROGRAM = bdb
SRC = bdb.c cache.c file-storage.c tree2dot.c main.c storage.c symbol.c
CFLAGS = -Wall -Werror -DTESTS
COPTFLAGS = -ggdb -g -O0

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(PROGRAM)

test: test/.passed

test/.passed:
	$(MAKE) -C test all

clean:
	rm -f $(OBJ) $(PROGRAM)
	$(MAKE) -C test clean

$(PROGRAM): $(OBJ)
	$(CC) -o $(PROGRAM) $(CFLAGS) $(OBJ)

%.o : %.c
	$(CC) $(CFLAGS) $(COPTFLAGS) -c $< -o $@
