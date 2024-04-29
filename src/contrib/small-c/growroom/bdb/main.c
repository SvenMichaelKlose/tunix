/////////////////
/// SYMBOL DB ///
/////////////////

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"

typedef struct _symbol {
    unsigned int  value;
    char          name[1];
} symbol;

int
symdb_compare (bdb *db, void *rec, void *key)
{
    symbol *s = rec;
    (void) db;
    printf ("Comparing \"%s\" & \"%s\".\n",
            (char *) key, s->name);
    return strcmp (s->name, key);
}

void *
symdb_data2key (void *rec)
{
    symbol *s = rec;
    return &s->name;
}

FILE * storage = NULL;

int
symdb_write (bdb *db, dbid_t ofs, void *r, size_t size)
{
    if (!storage)
        if (!(storage = fopen ("symbol.db", "w+")))
            perror ("Cannot open BDB file for writing.");
    fseek (storage, ofs, SEEK_SET);
    return fwrite (r, 1, size, storage);
}

int
symdb_read (bdb *db, dbid_t ofs, void *r, size_t size)
{
    if (!storage)
        return 0;
    fseek (storage, ofs, SEEK_SET);
    return fread (r, 1, size, storage);
}

bdb symdb;

int
add_symbol (char *name, int value)
{
    size_t size = sizeof (symbol) + strlen (name);
    symbol *s = malloc (size);
    printf ("Adding symbol \"%s\".\n", name);
    strcpy (s->name, name);
    s->value = value;
    return bdb_add (&symdb, name, s, size);
}

symbol *
find_symbol (char *name)
{
    dbid_t id = bdb_find (&symdb, name);
    if (id >= 0)
        return bdb_map (&symdb, id);
    return NULL;
}

void
symdb_init (void)
{
    symdb.next_free = 0;
    symdb.read    = symdb_read;
    symdb.write   = symdb_write;
    symdb.compare = symdb_compare;
    symdb.data2key = symdb_data2key;
}

///////////
/// APP ///
///////////

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;
    dbid_t id;
    symbol * s;
    char **n;

    symdb_init ();

    char *names[] = {
        "Coziness", "Fireplace", "Warmth", "Comfort", "Blanket",
        "Teatime", "Homecoming", "Family time", "Relaxation", "VIC-20",
        NULL
    };
    for (n = names; *n; n++)
        id = add_symbol (*n, strlen (*n));

    s = bdb_map (&symdb, id);
    printf ("Got symbol \"%s\".\n", s->name);

    s = find_symbol ("Homecoming");
    if (s)
        printf ("Got symbol \"%s\".\n", s->name);
    return 0;
}
