PROGRAM = bdb
SRC = bdb.c cache.c file-storage.c tree2dot.c main.c storage.c symbol.c
CFLAGS = -Wall -Werror -DTESTS
COPTFLAGS = -ggdb -g -O0

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(PROGRAM) test

test: test/.passed

test/.passed:
	$(MAKE) -C test all UNITY_ROOT="$(UNITY_ROOT)"

clean:
	rm -f $(OBJ) $(PROGRAM) symbol.bdb
	$(MAKE) -C test clean UNITY_ROOT="$(UNITY_ROOT)"

$(PROGRAM): $(OBJ)
	$(CC) -o $(PROGRAM) $(CFLAGS) $(OBJ)

%.o : %.c
	$(CC) $(CFLAGS) $(COPTFLAGS) -c $< -o $@
