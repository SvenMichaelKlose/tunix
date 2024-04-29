#include <stdbool.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdio.h>

#include "bdb.h"
#include "cache.h"
#include "storage.h"

dbid_t
bdb_add (bdb *db, void *key, void *data, size_t size)
{
    // Allocate ID (and space on storage).
    dbid_t id = storage_alloc_id (db, size);

    // Add record to cache.
    cnode *cn = cache_alloc (db, id, data, size);

    // Index cached record.
    cache_insert_key (db, cn);
    cache_insert_id (db, cn);

    return id;
}

// Find record by key.
dbid_t
bdb_find (bdb *db, void *key)
{
    cnode * cn = cache_find_key (db, key);
    return cn ?
        cn->id :
        storage_find (db, key);
}

// Map record by ID.
void *
bdb_map (bdb *db, int id)
{
    // Search for ID in cache.
    cnode *cn = cache_find_id (db, id);
    if (!cn)
        // Not found.  Fetch from storage.
        cn = cache_add_storage (db, id);
    else
        // Found.  Make it the most-recently used.
        cache_make_mru (cn);

    // Return record pointer.
    return cn->data;
}
