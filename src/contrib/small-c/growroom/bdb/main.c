#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"
#include "symbol.h"

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;
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
    for (n = names; *n; n++)
        id = add_symbol (*n, strlen (*n));

    s = bdb_map (&symdb, id);
    printf ("Got symbol \"%s\".\n", s->name);

    s = find_symbol (name);
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    else
        printf ("Symbol \"%s\" not found.\n", name);
    return 0;
}
