#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "bdb.h"
#include "cache.h"
#include "storage.h"

dbid_t
bdb_add (bdb *db, void *key, void *data, size_t size)
{
    dbid_t id = storage_alloc_id (db, size);
    cache_add (db, id, data, size);
    return id;
}

// Find record by key.
dbid_t
bdb_find (bdb *db, void *key)
{
    cnode * cn = cache_find_key (db, key);
    return cn ? cn->id : storage_find (db, key);
}

// Map record by ID.
// The record must exist.
void *
bdb_map (bdb *db, dbid_t id)
{
    cnode *cn = cache_find_id (db, id);
    if (!cn)
        cn = cache_add_stored (db, id);
    else
        cache_make_mru (db, cn);
    return cn->data;
}

void
bdb_flush (bdb *db)
{
    cache_flush (db);
}
