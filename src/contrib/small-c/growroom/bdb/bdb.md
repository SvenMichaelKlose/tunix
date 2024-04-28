File database
=============

This database holds records in memory
and moves least recently used ones to
secondary storage if that runs low.
Records have variable sizes and unique
keys and IDs for fast look-ups.  They
cannot be removed.

Pointers to records can be
requested with db_map() by passing it
the record ID.  The record then is
marked as being the most-recently used
one, so it'll move to secondary storage
if it is (most probably) not used again.
The minimum number of records in memory
should accordingly be as large as the
number of records the application must
be able to handle simultaneaously, or
the database is unusable.

Records can be added with db_add().  But
only modifications db_map()'ed records
will last.  db_find() does a search for
a particular key, based on the provided
test function which must behave like
strcmp().

Secondary storage is assumed to be a
block of memory that grows automatically
on writes beyond its size.

# A Symbol Database

This is a database to maintain a set of
symbols of variable size.

~~~C
/////////////////
/// SYMBOL DB ///
/////////////////

typedef struct _symbol {
    unsigned int  value;
    char          name[1];
} symbol;

int
symdb_test (struct symbol *a, struct symbol *b)
{
    return strcmp (a->name, b->name);
}

FILE * storage = NULL;

int
symdb_write (db *db, int ofs, void *r, int len)
{
    if (!storage)
        storage = fopen ("symbol.db", "rw");
    fposition (storage, ofs);
    fwrite (storage, ofs, 1, sizeof (int), &len);
    fwrite (storage, ofs, 1, len, r);
}

int
symdb_read (db *db, int ofs, void *r)
{
    fposition (storage, ofs);
    fread (storage, 1, sizeof (int), &len);
    fread (storage, 1, len, r);
}

db symdb = {
    .cache_size  = 64;
    .read        = symdb_read;
    .write       = symdb_write;
    .test        = symdb_test;
} db;

///////////
/// APP ///
///////////

void
init_symbols (void)
{
    db_open (db);
}

int
add_symbol (symbol *s)
{
    int len = sizeof (symbol) + strlen (s->name);
    return (db_add (symdb, s, len);
}

symbol = db_read (db, index);
symbol = db_find (db, sname);
if (s == -1)
    error ();
~~~

# B-tree indexed cache

Records in memory and secondary storage
are separately indexed using b-trees.
