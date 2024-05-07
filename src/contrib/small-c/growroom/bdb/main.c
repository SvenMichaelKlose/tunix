#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"
#include "cache.h"
#include "symbol.h"
#include "tree2dot.h"

void
symbol_tests (void)
{
    dbid_t id;
    symbol * s;
    char **n;
    char *name = "Homecoming";
    FILE * dot;

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

    s = find_symbol (name);
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    else
        printf ("Symbol \"%s\" not found.\n", name);

    dot = fopen ("symbol.dot", "w");
    tree2dot (dot, symdb.cache_root_keys);
    fclose (dot);
    symbol_close ();
}

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    symbol_tests ();

    return 0;
}
