#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "symbol.h"

#include "unity.h"

void
symbol_tests (void)
{
    dbid_t id;
    symbol * s;
    char **n;
    char *name = "Homecoming";

    symbol_init ();

    char *names[] = {
        "Coziness", "Fireplace", "Warmth", "Comfort", "Blanket",
        "Teatime", "Homecoming", "Family time", "Relaxation", "VIC-20",
        NULL
    };
    for (int i = 0; i < 20; i++)
        for (n = names; *n; n++)
            id = add_symbol (*n, strlen (*n));

    s = bdb_map (&symdb, id);
    printf ("Got symbol \"%s\".\n", s->name);
exit (0);
    s = find_symbol (name);
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    else
        printf ("Symbol \"%s\" not found.\n", name);

    symbol_close ();
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("test/basic-lru-list.c");
  RUN_TEST(symbol_tests, 0);
  return UnityEnd();
}
