#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

// Pop least-recently used off LRU list.
cnode *
cache_pop_lru (bdb *db)
{
    cnode * cn = db->cache_last;
    if (cn)
        db->cache_last = cn->prev;
    return cn;
}

// Remove from LRU list.
void
cache_remove_lru (bdb *db, cnode *cn)
{
    // Update link in previous node or
    // the pointer to the first node
    // (which is most-recently used).
    if (cn->prev)
        cn->prev->next = cn->next;
    else
        db->cache_first = cn->next;

    // Update link in next node or the
    // pointer to the last node (which
    // is the least-recently used one).
    if (cn->next)
        cn->next->prev = cn->prev;
    else
        db->cache_last = cn->prev;
}

// Move to front of LRU list (most-recent).
void
cache_make_mru (bdb *db, cnode *cn)
{
    // Remove from wherever node is in the list.
    cache_remove_lru (db, cn);

    // Push it onto the front of the list
    // (most-recently used).
    cache_push_mru (db, cn);
}

char
bit_reverse (char x)
{
    int i;
    char new;
    char mask = 1 << 7;
    for (i = 0; i < 8; i++) {
        new <<= 1;
        new |= x & mask ? 1 : 0;
        mask >>= 1;
    }
    return new;
}

// Turn ID into btree-compatible value:
// pseudo-random and unique.
dbid_t
cache_idhash (dbid_t id)
{
    dbid_t new;
    char * ip = (char *) &id;
    char * np = (char *) &new;
    char nbytes = sizeof (dbid_t);
    int i;

    // Signed divide to avoid sticky bits.
    id = (dbid_t) ((signed) id / sizeof (int));

    // Reverse order of bits.
    for (i = 0; i < nbytes; i++)
        np[nbytes - 1 - i] = bit_reverse (ip[i]);

    return new;
}

// Insert node into ID index.
void
cache_insert_id (bdb *db, cnode *cn)
{
    dbid_t  key = cache_idhash (cn->id);
    cnode   *n  = db->cache_root_ids;
    cnode   *on;

    // Add first node.
    if (!n) {
        db->cache_root_ids = cn;
        return;
    }

    // Travel down binary tree.
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

// Insert node into key index.
void
cache_insert_key (bdb *db, cnode *cn)
{
    void    *key = db->data2key (cn->data);
    cnode   *n   = db->cache_root_keys;
    cnode   *on;

    // Add first node.
    if (!n) {
        db->cache_root_keys = cn;
        return;
    }

    // Travel down binary tree.
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

// Find by ID.
cnode *
cache_find_id (bdb *db, dbid_t id)
{
    dbid_t  key = cache_idhash (id);
    cnode   *n  = db->cache_root_ids;

    // Empty tree, nothing to find.
    if (!n)
        return NULL;

    // Travel down binary tree.
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

// Find by key.
cnode *
cache_find_key (bdb *db, void *key)
{
    cnode *n = db->cache_root_keys;
    int c;

    // Empty tree, nothing to find.
    if (!n)
        return NULL;

    for (;;) {
        // Test and return if match.
        if (!(c = db->compare (db, n->data, key)))
            return n;

        // Travel down the tree.
        if (c < 0) {
            if (!(n = n->kleft))
                return NULL;
        } else
            if (!(n = n->kright))
                return NULL;
    }
}

// Remove from cache.
void
cache_remove (bdb *db, cnode *cn)
{
    cache_remove_lru (db, cn);
    //cache_remove_id (db, cn);
    //cache_remove_key (db, cn);
    free (cn);
}

// Move least-recently used record to storage.
void
cache_store_lru (bdb *db)
{
    // Get least-recently used record.
    cnode *cn = cache_pop_lru (db);

    // Insert into b-tree if new on storage.
    if (!(cn->flags & HAS_STORAGE))
        storage_insert_key (db, db->data2key (cn->data), cn->id);

    // Write record data.  Updates happen here, too.
    storage_write_data (db, cn->id, cn->data, cn->size);

    // Remove record from cache.
    cache_remove (db, cn);
}

// Add new record to cache.
cnode *
cache_add (bdb *db, dbid_t id, void *data, size_t size)
{
    cnode *cn;

    // When out of cache...
    if (num_cached > 256)
        // Move least-recently used record to storage.
        cache_store_lru (db);
    else
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

    // Add to key index.
    cache_insert_key (db, cn);

    // Add to ID index.
    cache_insert_id (db, cn);
    return cn;
}

// Fetch record from storage and add it to the cache.
cnode *
cache_add_stored (bdb *db, dbid_t id)
{
    snode  *bn;
    size_t size;

    // Map from secondary storage.
    if (!(bn = storage_map (&size, db, id)))
        return NULL;

    // Allocate a new cnode + data memory.
    return cache_add (db, id, &bn->data, size);
}
