UNITY_SRC = ../../../contrib/unity/src

PROGRAMS = \
	storage-layout \
	basic-lru-list \
	cnode-inserts \
	symbol
COMMONSOURCES = \
	../bdb.c \
	../cache.c \
	../file-storage.c \
	../storage.c \
	../symbol.c \
	$(UNITY_SRC)/unity.c

COPTFLAGS = -ggdb -g -O0
CFLAGS = -Wall -Werror \
	-I. -I.. -I$(UNITY_SRC) \
	-DTESTS

COMMONOBJ = $(COMMONSOURCES:.c=.o)

all: $(PROGRAMS) test

test: .passed

.passed:
	@for i in $(PROGRAMS); do \
	    echo "Running test '$$i'."; \
	    (./$$i && exit); \
	    if [ $$? -ne 0 ]; then \
	        echo "Tests failed!"; exit 1; \
	    fi \
	done
	touch .passed

$(PROGRAMS): $(COMMONOBJ)

clean:
	$(RM) $(COMMONOBJ) $(PROGRAMS:.c=.o) $(PROGRAMS) $(PROGRAMS:=.bdb) .passed

%: %.o $(COMMONOBJ)
	$(CC) -o $@ $(CFLAGS) $(COPTFLAGS) $<

%.o: %.c
	$(RM) .passed
	$(CC) -o $@ $(CFLAGS) $(COPTFLAGS) -c $<
