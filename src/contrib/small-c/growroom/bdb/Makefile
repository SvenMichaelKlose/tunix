PROGRAM = bdb
SRC = api.c cache.c main.c storage.c symbol.c
INCDIR = "./include/"
CFLAGS = -ggdb -g -O0 -Wall -Werror

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: $(PROGRAM)

clean:
	rm -f $(OBJ) $(PROGRAM)

$(PROGRAM): $(OBJ)
	$(CC) -o $(PROGRAM) $(CFLAGS) $(OBJ)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
