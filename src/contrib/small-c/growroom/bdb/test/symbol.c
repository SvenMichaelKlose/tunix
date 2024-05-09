#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "symbol.h"

#include "unity.h"

extern bdb symdb;

char *names[] = {
    "Coziness", "Fireplace", "Warmth", "Comfort", "Blanket",
    "Teatime", "Homecoming", "Family time", "Relaxation", "VIC-20",
    NULL
};

void
find_inserted_symbols (void)
{
    symbol * s;
    char **n;
    for (n = names; *n; n++) {
        s = find_symbol (*n);
        TEST_ASSERT_MESSAGE(s, *n);
    }
}

void
symbol_tests (void)
{
    char **n;
    dbid_t id;

    symbol_init ();
    for (n = names; *n; n++) {
        id = add_symbol (*n, strlen (*n));
        TEST_ASSERT_MESSAGE(id != ERROR, *n);
    }

    find_inserted_symbols ();
    symbol_flush ();
    TEST_ASSERT(symdb.cache_root_keys == 0);
    TEST_ASSERT(symdb.cache_root_ids == 0);
    find_inserted_symbols ();
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("test/symbol.c");
  RUN_TEST(symbol_tests, 1);
  return UnityEnd();
}
