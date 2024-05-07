#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#include "bdb.h"
#include "storage.h"
#include "cache.h"

cnode *
cnode_alloc (void)
{
    return calloc (1, sizeof (cnode));
}

// Add node as most-recently used.
void
cache_push_mru (bdb *db, cnode *cn)
{
    cn->prev = NULL;
    if (db->cache_mru)
        db->cache_mru->prev = cn;
    cn->next = db->cache_mru;
    db->cache_mru = cn;
    if (!db->cache_lru)
        db->cache_lru = cn;
}

// Remove from LRU list.
void
cache_remove_lru (bdb *db, cnode *cn)
{
    // Update previous node or the
    // pointer to the first node in the
    // list (nost-recently used).
    if (cn->prev)
        cn->prev->next = cn->next;
    else
        db->cache_mru = cn->next;

    // Update next node or the pointer
    // to the last node in the list.
    // (least-recently used).
    if (cn->next)
        cn->next->prev = cn->prev;
    else
        db->cache_lru = cn->prev;
}

// Pop least-recently used off LRU list.
cnode *
cache_pop_lru (bdb *db)
{
    cnode * cn = db->cache_lru;
    if (cn)
        cache_remove_lru (db, cn);
    return cn;
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

// Reverse order of bits in byte.
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
    return id;
/*
    dbid_t new;
    char * ip = (char *) &id;
    char * np = (char *) &new;
    char nbytes = sizeof (dbid_t);
    int i;

    // Signed divide to avoid sticky bits.
    id = (dbid_t) ((signed) id / sizeof (int));

    // Reverse order of bits and bytes.
    for (i = 0; i < nbytes; i++)
        np[nbytes - 1 - i] = bit_reverse (ip[i]);

    return new;
*/
}

// Insert node into ID index.
void
cache_insert_id (bdb *db, cnode *cn)
{
    dbid_t  key = cache_idhash (cn->id);
    cnode   *n  = db->cache_root_ids;
    cnode   *on = (void *) -1;

    // Add first node.
    if (!n) {
        db->cache_root_ids = cn;
        cn->iparent = NULL;
        return;
    }

    // Travel down binary tree.
    for (;;) {
        on = n;
        if (cache_idhash (n->id) < key) {
            if (!(n = n->ileft)) {
                on->ileft = cn;
                break;
            }
        } else
            if (!(n = n->iright)) {
                on->iright = cn;
                break;
            }
    }
    cn->iparent = on;
}

void
cache_remove_id (bdb *db, cnode *cn)
{
    cnode * p = cn->iparent;

    // Unlink from parent.
    if (p) {
        if (p->ileft == cn)
            p->ileft = NULL;
        else
            p->iright = NULL;
    } else
        db->cache_root_ids = NULL;

    // Re-insert child nodes.
    if (cn->ileft)
        cache_insert_key (db, cn->ileft);
    if (cn->iright)
        cache_insert_key (db, cn->iright);
}

// Insert node into key index.
void
cache_insert_key (bdb *db, cnode *cn)
{
    void    *key = db->data2key (cn->data);
    cnode   *n   = db->cache_root_keys;
    cnode   *on = (void *) -1;

    // Add first node.
    if (!n) {
        db->cache_root_keys = cn;
        cn->kparent = NULL;
        return;
    }

    // Travel down binary tree.
    for (;;) {
        if (on == n)
            err (EXIT_FAILURE, "cache_insert_key(): endless loop");
        on = n;
        if (db->compare (db, n->data, key) < 0) {
            if (!(n = n->kleft)) {
                on->kleft = cn;
                break;
            }
        } else
            if (!(n = n->kright)) {
                on->kright = cn;
                break;
            }
    }
    cn->kparent = on;
}

void
cache_remove_key (bdb *db, cnode *cn)
{
    cnode * p = cn->kparent;

    // Unlink from parent.
    if (p) {
        if (p->kleft == cn)
            p->kleft = NULL;
        else
            p->kright = NULL;
    } else
        db->cache_root_keys = NULL;

    // Re-insert child nodes.
    if (cn->kleft)
        cache_insert_key (db, cn->kleft);
    if (cn->kright)
        cache_insert_key (db, cn->kright);
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
    cache_remove_id (db, cn);
    cache_remove_key (db, cn);
    free (cn->data);
    free (cn);
}

// Fetch record from storage and add it to the cache.
cnode *
cache_add_stored (bdb *db, dbid_t id)
{
    snode  *bn;
    cnode  *cn;
    size_t size;

    // Map from secondary storage.
    if (!(bn = storage_map (&size, db, id)))
        return NULL;

    // Allocate a new cnode + data memory.
    cn = cache_add (db, id, &bn->data, size);

    // Tell that it's on storage already.
    cn->flags |= HAS_STORAGE;

    return cn;
}

// Move least-recently used record to storage.
void
cache_store_lru (bdb *db)
{
    printf ("cache_store_lru ()\n");

    // Get least-recently used record.
    cnode *cn = cache_pop_lru (db);
    if (!cn)
        perror ("cache_store_lru() called without cached records.");

    // Update data or write new storage record.
    if (cn->flags & HAS_STORAGE)
        storage_write_data (db, cn->id, cn->data, cn->size);
    else
        storage_add (db, cn->id, cn->data, cn->size);

    // Remove record from cache.
    cache_remove (db, cn);
}

// Add new record to cache.
cnode *
cache_add (bdb *db, dbid_t id, void *data, size_t size)
{
    cnode *cn;

    // When out of cache...
    if (db->num_cached == BDB_MAX_CACHED)
        // Move least-recently used record to storage.
        cache_store_lru (db);
    else
        db->num_cached++;

    // Allocate cnode.
    if (!(cn = cnode_alloc ()))
        return NULL;

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

void
cache_flush (bdb *db)
{
    printf ("cache-flush() %d records.\n",
            db->num_cached);
    while (db->num_cached--)
        cache_store_lru (db);
}
