#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"
#include "cache.h"
#include "symbol.h"

void
symbol_tests (void)
{
    dbid_t id;
    symbol * s;
    char **n;
    char *name = "Homecoming";

    symbol_init ();

    char *names[] = {
        "Coziness", "Fireplace", "Warmth", NULL, "Comfort", "Blanket",
        "Teatime", "Homecoming", "Family time", "Relaxation", "VIC-20",
        NULL
    };
    //for (int i = 0; i < 30; i++)
        for (n = names; *n; n++)
            id = add_symbol (*n, strlen (*n));

    s = bdb_map (&symdb, id);
    printf ("Got symbol \"%s\".\n", s->name);

    s = find_symbol (name);
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    else
        printf ("Symbol \"%s\" not found.\n", name);

    symbol_close ();
}

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    cache_tests ();
    symbol_tests ();

    return 0;
}
