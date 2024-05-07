ifndef $(UNITY_ROOT)
    $(warn UNITY_ROOT is not set. git@github.com:ThrowTheSwitch/Unity.git)
endif

PROGRAMS = \
	storage-layout \
	basic-lru-list \
	cnode-inserts \
	symbol
COMMONSOURCES = \
	../api.c \
	../cache.c \
	../file-storage.c \
	../storage.c \
	../symbol.c \
	$(UNITY_ROOT)/unity.c

COPTFLAGS = -ggdb -g -O0
CFLAGS = -Wall -Werror \
	-I. -I.. -I$(UNITY_ROOT) \
	-DTESTS

COMMONOBJ = $(COMMONSOURCES:.c=.o)

all: $(PROGRAMS) test

test: .passed

.passed:
	@for i in $(PROGRAMS); do \
	    echo "Running test '$$i'."; \
	    (./$$i && exit); \
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
