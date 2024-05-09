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
    int rel;
    (void) db;
    rel = strcmp (key, s->name);
    return rel;
}

void *
symdb_data2key (void *rec)
{
    symbol *s = rec;
    return &s->name;
}

bdb symdb;

dbid_t
add_symbol (char *name, int value)
{
    size_t size = sizeof (symbol) + strlen (name);
    symbol *s   = malloc (size);
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
    symdb.storage   = "symbol.bdb";
    symdb.read      = bdb_file_read;
    symdb.write     = bdb_file_write;
    symdb.compare   = symdb_compare;
    symdb.data2key  = symdb_data2key;
}

void
symbol_flush ()
{
    bdb_flush (&symdb);
}
