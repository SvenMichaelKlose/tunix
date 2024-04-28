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
symdb_test (void *rec, void *key)
{
    symbol *s = rec;
    return strcmp (s->name, key);
}

FILE * storage = NULL;

int
symdb_write (bdb *db, dbid_t ofs, void *r, size_t size)
{
    if (!storage)
        storage = fopen ("symbol.db", "rw");
    fseek (storage, ofs, SEEK_SET);
    return fwrite (r, size, 1, storage);
}

int
symdb_read (bdb *db, dbid_t ofs, void *r, size_t size)
{
    fseek (storage, ofs, SEEK_SET);
    return fread (storage, size, 1, r);
}

bdb symdb;

int
add_symbol (symbol *s)
{
    size_t size = sizeof (symbol) + strlen (s->name);
    return bdb_add (&symdb, s->name, s, size);
}

///////////
/// APP ///
///////////

int
main (int argc, char *argv[])
{
    (void) argc;
    (void) argv;
    symdb.read  = symdb_read;
    symdb.write = symdb_write;
    symdb.test  = symdb_test;
    return 0;
}
