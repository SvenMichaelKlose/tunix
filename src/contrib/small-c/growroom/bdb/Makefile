export UNITY_ROOT

PROGRAM = bdb
SRC = api.c cache.c main.c storage.c symbol.c
INCDIR = "./include/"
CFLAGS = -ggdb -g -O0 -Wall -Werror -DTESTS

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(PROGRAM)

test:
	$(MAKE) -C test all

clean:
	rm -f $(OBJ) $(PROGRAM) symbol.bdb
	$(MAKE) -C test clean

$(PROGRAM): $(OBJ)
	$(CC) -o $(PROGRAM) $(CFLAGS) $(OBJ)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
