#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#include "bdb.h"
#include "storage.h"
#include "cache.h"

#include "symbol.h"

cnode *
cnode_alloc (void)
{
    return calloc (1, sizeof (cnode));
}

// Add node as most-recently used.
void
cache_push_mru (bdb *db, cnode *new)
{
    // Link formerly MRU to new.
    new->prev = NULL;
    new->next = db->cache_mru;
    if (db->cache_mru)
        db->cache_mru->prev = new;

    // Update MRU/LRU.
    db->cache_mru = new;
    if (!db->cache_lru)
        db->cache_lru = new;
}

// Remove from LRU list.
void
cache_list_remove (bdb *db, cnode *cn)
{
    cnode * prev = cn->prev;
    cnode * next = cn->next;

    if (prev)
        prev->next = next;
    else
        db->cache_mru = next;

    if (next)
        next->prev = prev;
    else
        db->cache_lru = prev;
}

// Pop least-recently used off LRU list.
cnode *
cache_pop_lru (bdb *db)
{
    cnode * cn = db->cache_lru;
    if (cn)
        cache_list_remove (db, cn);
    return cn;
}

// Move to front of LRU list (most-recent).
void
cache_make_mru (bdb *db, cnode *cn)
{
    cache_list_remove (db, cn);
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
        new |= (x & mask) ? 1 : 0;
        mask >>= 1;
    }
    return new;
}

// Turn ID into btree-compatible value:
// pseudo-random and unique.
dbid_t
cache_id2key (dbid_t id)
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
// https://en.wikipedia.org/wiki/Binary_search_tree
void
cache_insert_id (bdb *db, cnode *cn)
{
    dbid_t  key = cache_id2key (cn->id);
    cnode   *n  = db->cache_root_ids;
    cnode   *on = (void *) -1;

    // Add first node as root.
    if (!n) {
        db->cache_root_ids = cn;
        cn->iparent = NULL;
        return;
    }

    for (;;) {
        on = n;
        if (cache_id2key (n->id) < key) {
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
cache_index_remove_id (bdb *db, cnode *cn)
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

    // Re-insert children.
    if (cn->ileft)
        cache_insert_id (db, cn->ileft);
    if (cn->iright)
        cache_insert_id (db, cn->iright);
}

// Insert node into key index.
// https://en.wikipedia.org/wiki/Binary_search_tree
void
cache_insert_key (bdb *db, cnode *cn)
{
    void    *key = db->data2key (cn->data);
    cnode   *n   = db->cache_root_keys;
    cnode   *on  = (void *) -1;

    // Add first node as root.
    if (!n) {
        db->cache_root_keys = cn;
        cn->kparent = NULL;
        return;
    }

    for (;;) {
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
cache_index_remove_key (bdb *db, cnode *cn)
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

    // Re-insert children.
    if (cn->kleft)
        cache_insert_key (db, cn->kleft);
    if (cn->kright)
        cache_insert_key (db, cn->kright);
}

// Find by ID.
cnode *
cache_find_id (bdb *db, dbid_t id)
{
    dbid_t  key = cache_id2key (id);
    cnode   *n  = db->cache_root_ids;

    // Empty tree, nothing to find.
    if (!n)
        return NULL;

    // Travel down binary tree.
    for (;;) {
        if (n->id == id)
            return n;
        if (cache_id2key (n->id) < key) {
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

// Remove from cache.
void
cache_remove (bdb *db, cnode *cn)
{
    cache_list_remove (db, cn);
    cache_index_remove_id (db, cn);
    cache_index_remove_key (db, cn);
    free (cn->data);
    free (cn);
}

// Move least-recently used record to storage.
void
cache_swap_out_lru (bdb *db)
{
    // Get least-recently used record.
    cnode *cn = cache_pop_lru (db);
    assert(cn);

    // Update data or write new storage record.
    if (cn->flags & HAS_STORAGE)
        storage_write_data (db, cn->id, cn->data, cn->size);
    else
        storage_add (db, cn->id, cn->data, cn->size);

    cache_remove (db, cn);
}

// Add new record to cache.
cnode *
cache_add (bdb *db, dbid_t id, void *data, size_t size)
{
    cnode *cn;

    // Swap out if it's time.
    if (db->num_cached == BDB_MAX_CACHED)
        cache_swap_out_lru (db);
    else
        db->num_cached++;

    if (!(cn = cnode_alloc ()))
        return NULL;

    // Copy over record info and data.
    cn->id   = id;
    cn->size = size;
    cn->data = malloc (size);
    memcpy (cn->data, data, size);

    cache_push_mru (db, cn);
    cache_insert_key (db, cn);
    cache_insert_id (db, cn);

    return cn;
}

void
cache_flush (bdb *db)
{
    while (db->num_cached--)
        cache_swap_out_lru (db);
}

// Fetch record from storage and add it to the cache.
cnode *
cache_add_stored (bdb *db, dbid_t id)
{
    snode  *sn;
    size_t size;
    cnode  *cn;

    printf ("add_stored_map() %d\n", id);
    if ((sn = storage_map (&size, db, id))) {
        cn = cache_add (db, id, &sn->data, size);
    printf ("In %d,%s\n", id, ((symbol *) cn->data)->name);
        cn->flags |= HAS_STORAGE;
        free (sn);
        return cn;
    }
    return NULL;
}
