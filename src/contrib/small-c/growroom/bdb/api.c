#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "bdb.h"
#include "cache.h"
#include "storage.h"

dbid_t
bdb_add (bdb *db, void *key, void *data, size_t size)
{
    // Allocate ID and space on storage.
    dbid_t id = storage_alloc_id (db, size);

    // Add record to cache and update index.
    if (!cache_add (db, id, data, size))
        perror ("bdb_add(): Cannot allocate storage.");

    return id;
}

// Find record by key.
dbid_t
bdb_find (bdb *db, void *key)
{
    // First check the cache.
    cnode * cn = cache_find_key (db, key);

    // Return found or find on storage.
    return cn ?
        cn->id :
        storage_find (db, key);
}

// Map record by ID.
// The record must exist.
void *
bdb_map (bdb *db, int id)
{
    // Search for ID in cache.
    cnode *cn = cache_find_id (db, id);
    if (!cn)
        // Not found.  Fetch from storage.
        cn = cache_add_stored (db, id);
    else
        // Found.  Make it the most-recently used.
        cache_make_mru (db, cn);

    // Return record pointer.
    return cn->data;
}

void
bdb_close (bdb *db)
{
    cache_flush (db);
}
