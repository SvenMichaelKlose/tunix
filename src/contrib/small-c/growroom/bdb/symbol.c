#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#include "bdb.h"
#include "symbol.h"

bdb symdb;

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
symdb_write (bdb *db, dbid_t ofs, void *data, size_t size)
{
    char * filename = "symbol.bdb";
    if (!storage)
        if (!(storage = fopen (filename, "w+")))
            perror ("Cannot open file '%s' for writing.");
    printf ("WR: %04x, %ld\n", ofs, size);
    if (fseek (storage, ofs, SEEK_SET) < 0)
        perror ("symdb_write(): cannot seek.");
    return fwrite (data, 1, size, storage);
}

int
symdb_read (bdb *db, dbid_t ofs, void *data, size_t size)
{
    if (!storage)
        return 0;
    printf ("RD: %04x, %ld\n", ofs, size);
    if (fseek (storage, ofs, SEEK_SET) < 0)
        perror ("symdb_read(): cannot seek.");
    return fread (data, 1, size, storage);
}

bdb symdb;

dbid_t
add_symbol (char *name, int value)
{
    size_t size = sizeof (symbol) + strlen (name);
    symbol *s   = malloc (size);
    printf ("Adding symbol \"%s\".\n", name);
    strcpy (s->name, name);
    s->value = value;
    return bdb_add (&symdb, name, s, size);
}

symbol *
find_symbol (char *name)
{
    dbid_t id = bdb_find (&symdb, name);
    if (id != NOTFOUND)
        return bdb_map (&symdb, id);
    return NULL;
}

void
symbol_init ()
{
    symdb.next_free = 0;
    symdb.read      = symdb_read;
    symdb.write     = symdb_write;
    symdb.compare   = symdb_compare;
    symdb.data2key  = symdb_data2key;
}

void
symbol_close ()
{
    bdb_close (&symdb);
}
