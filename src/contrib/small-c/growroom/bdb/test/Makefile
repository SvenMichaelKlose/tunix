ifndef $(UNITY_ROOT)
    $(warn UNITY_ROOT is not set. git@github.com:ThrowTheSwitch/Unity.git)
endif

PROGRAMS = basic-lru-list symbol
COMMONSOURCES = \
	../api.c \
	../cache.c \
	../storage.c \
	../symbol.c \
	$(UNITY_ROOT)/unity.c

CFLAGS = -ggdb -g -O0 -Wall -Werror \
	-I. -I.. -I$(UNITY_ROOT) \
	-DTESTS
COMMONOBJ = $(COMMONSOURCES:.c=.o)

all: $(PROGRAMS) test

test: .passed

.passed:
	@for i in $(PROGRAMS); do \
	    echo "Running test '$$i'."; \
	    (./$$i || exit); \
	done
	touch .passed

$(PROGRAMS): $(COMMONOBJ)

clean:
	$(RM) $(COMMONOBJ) $(PROGRAMS:.c=.o) $(PROGRAMS) symbol.bdb .passed

%: %.o $(COMMONOBJ)
	$(CC) -o $@ $(CFLAGS) $<

%.o: %.c
	$(RM) .passed
	$(CC) $(CFLAGS) -c $< -o $@
