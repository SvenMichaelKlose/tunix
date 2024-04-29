#include <stdbool.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdio.h>

#include "bdb.h"
#include "storage.h"
#include "cache.h"

int num_cached = 0;

// Add node as most-recently used.
void
cache_push_mru (bdb *db, cnode *cn)
{
    cn->prev = NULL;
    cn->next = db->cache_first;
    db->cache_first = cn;
    if (!db->cache_last)
        db->cache_last = cn;
}

// Pop least-recently used.
cnode *
cache_pop_lru (bdb *db)
{
    cnode * cn = db->cache_last;
    if (cn)
        db->cache_last = cn->prev;
    return cn;
}

void
cache_remove_lru (bdb *db, cnode *cn)
{
    if (cn->prev)
        cn->prev->next = cn->next;
    else
        db->cache_first = cn->next;
    if (cn->next)
        cn->next->prev = cn->prev;
    if (cn == db->cache_last)
        db->cache_last = cn->prev;
}

void
cache_make_mru (bdb *db, cnode *cn)
{
    cache_remove_lru (db, cn);
    cache_push_mru (db, cn);
}

dbid_t
cache_idhash (dbid_t id)
{
    // TODO: Divide signed to filter alignment and
    // reverse bit order to ensure a balanced tree. (smk)
    return id;
}

void
cache_insert_id (bdb *db, cnode *cn)
{
    dbid_t  key = cache_idhash (cn->id);
    cnode   *n = db->cache_root_ids;
    cnode   *on;

    if (!n) {
        db->cache_root_ids = cn;
        return;
    }
    for (;;) {
        on = cn;
        if (cache_idhash (n->id) < key) {
            if (!(n = n->ileft)) {
                on->ileft = cn;
                return;
            }
        } else
            if (!(n = n->iright)) {
                on->iright = cn;
                return;
            }
    }
}

void
cache_insert_key (bdb *db, cnode *cn)
{
    void    *key = db->data2key (cn->data);
    cnode   *n = db->cache_root_keys;
    cnode   *on;

    if (!n) {
        db->cache_root_keys = cn;
        return;
    }
    for (;;) {
        on = cn;
        if (db->compare (db, n->data, key) < 0) {
            if (!(n = n->kleft)) {
                on->kleft = cn;
                return;
            }
        } else
            if (!(n = n->kright)) {
                on->kright = cn;
                return;
            }
    }
}

cnode *
cache_find_id (bdb *db, dbid_t id)
{
    dbid_t  key = cache_idhash (id);
    cnode   *n = db->cache_root_ids;

    if (!n)
        return NULL;
    for (;;) {
        if (n->id == id)
            return n;
        if (cache_idhash (n->id) < key) {
            if (!(n = n->ileft))
                return NULL;
        } else
            if (!(n = n->iright))
                return NULL;
    }
}

cnode *
cache_find_key (bdb *db, void *key)
{
    cnode *n = db->cache_root_keys;
    int c;

    if (!n)
        return NULL;
    for (;;) {
        if (!(c = db->compare (db, n->data, key)))
            return n;
        if (c < 0) {
            if (!(n = n->kleft))
                return NULL;
        } else
            if (!(n = n->kright))
                return NULL;
    }
}

void
cache_remove (bdb *db, cnode *cn)
{
    cache_remove_lru (db, cn);
    //cache_remove_id (db, cn);
    //cache_remove_key (db, cn);
    free (cn);
}

cnode *
cache_alloc (bdb *db, dbid_t id, void *data, size_t size)
{
    cnode *cn;

    // When out of cache...
    if (num_cached > 256) {
        // ...get least-recently used cnode...
        cn = cache_pop_lru (db);

        // Insert into b-tree if new on storage.
        if (!(cn->flags & CNODE_HAS_BNODE))
            storage_insert_key (db, db->data2key (cn->data), cn->id);

        // Init/update record data.
        storage_write_data (db, cn->id, cn->data, cn->size);

        // Remove record from cache.
        cache_remove (db, cn);
    } else
        num_cached++;

    // Allocate and clear cnode.
    cn = malloc (sizeof (cnode));
    bzero (cn, sizeof (cnode));

    // Copy over record info and data.
    cn->id   = id;
    cn->size = size;
    cn->data = malloc (size);
    memcpy (cn->data, data, size);

    // Add to LRU list as most-recently used.
    cache_push_mru (db, cn);
    cache_insert_key (db, cn);
    cache_insert_id (db, cn);
    return cn;
}


cnode *
cache_add_storage (bdb *db, dbid_t id)
{
    snode  *bn;
    cnode  *cn;
    size_t size;

    // Map from secondary storage.
    if (!(bn = storage_map (&size, db, id)))
        return NULL;

    // Allocate a new cnode + data memory.
    cn = cache_alloc (db, id, &bn->data, size);

    // Insert cnode into b-trees.
    cache_insert_key (db, cn);
    cache_insert_id (db, cn);

    return cn;
}
