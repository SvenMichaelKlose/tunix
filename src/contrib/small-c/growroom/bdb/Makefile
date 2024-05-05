PROGRAM = bdb
SRC = api.c cache.c main.c storage.c symbol.c
CFLAGS = -ggdb -g -O0 -Wall -Werror -DTESTS

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
	$(CC) $(CFLAGS) -c $< -o $@
