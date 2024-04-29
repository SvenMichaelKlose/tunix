#include <stdbool.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdio.h>

#include "bdb.h"

/////////////////////////
/// SECONDARY STORAGE ///
/////////////////////////

// Write head of bnode only.
void
bnode_write_bnode (bdb *db, dbid_t id, bnode * n)
{
    size_t nwritten = db->write (db, id + sizeof (size_t), n, sizeof (bnode));
    if (nwritten != sizeof (bnode))
        perror ("Error writing data.");
}

// Allocate ID for new record.
dbid_t
bnode_alloc_id (bdb *db, size_t size)
{
    dbid_t id = db->next_free;

    // Make space for size, bnode and data.
    db->next_free += size + sizeof (bnode) + sizeof (size_t);
    return id;
}

// Write size before bnode to storage.
bool
bnode_write_size (bdb *db, dbid_t id, size_t size)
{
    size_t nsize = sizeof (bnode) + size;
    size_t nwritten = db->write (db, id, &nsize, sizeof (size_t));
    return nwritten != sizeof (size_t);
}

// Write data after bnode to storage.
bool
bnode_write_data (bdb *db, dbid_t id, void * data, size_t size)
{
    size_t nsize = sizeof (size_t) + sizeof (bnode);
    size_t nwritten = db->write (db, nsize, data, size);
    return nwritten != nsize;
}

// Allocate new record on storage.
dbid_t
bnode_alloc (bdb *db, void * data, size_t size)
{
    dbid_t id = bnode_alloc_id (db, size);
    if (!bnode_write_size (db, id, size))
        perror ("Error writing bnode size.");
    if (!bnode_write_data (db, id, data, size))
        perror ("Error writing bnode data.");
    return id;
}

bnode *
bnode_map (size_t *size, bdb *db, int id)
{
    bnode * n;
    size_t nsize;
    size_t nread;

    // Read size of node.
    nread = db->read (db, id, &nsize, sizeof (size_t));
    if (!id && !nread)
        return NULL;  // Missing root node.
    *size = nsize - sizeof (bnode);

    // Allocate memory for node & data.
    if (!(n = malloc (nsize)))
        return NULL;

    // Read node & data.
    nread = db->read (db, id + sizeof (size_t), n, nsize);
    if (nread != nsize)
        perror ("bnode_map(): Reading data failed.");

    return n;
}

void
bnode_insert_key (bdb *db, void *key, dbid_t recid)
{
    bnode   *n;
    dbid_t  id = 0;
    dbid_t  oid = 0;
    size_t  unused_size;

    for (;;) {
        // Map node into memory.
        if (!(n = bnode_map (&unused_size, db, id)))
            return; // Root node.

        // Decide which to child to travel.
        oid = id;  // Save for update.
        if (db->compare (db, &n->data, key) < 0) {
            // Go left.
            if (!(id = n->left)) {
                // Set left child.
                n->left = recid;
                bnode_write_bnode (db, oid, n);
                return;
            }
        } else
            // Go right.
            if (!(id = n->right)) {
                // Set right child.
                n->right = recid;
                bnode_write_bnode (db, oid, n);
                return;
            }
    }
}

dbid_t
bnode_add (bdb *db, void *key, void *data, size_t size)
{
    dbid_t id = bnode_alloc (db, data, size);
    bnode_insert_key (db, key, id);
    return id;
}

dbid_t
bnode_find (bdb *db, void *key)
{
    bnode *n;
    dbid_t id = 0;
    int c;
    size_t unused_size;

    for (;;) {
        // Map node into memory.
        if (!(n = bnode_map (&unused_size, db, id)))
            perror ("Internal error. Node not mapped.");

        // Decide to which child to travel.
        if (!(c = db->compare (db, &n->data, key)))
            return id;  // Match!
        if (c < 0) {
            // Go left.
            if (!(id = n->left))
                return NOTFOUND;
        } else
            // Go right.
            if (!(id = n->right))
                return NOTFOUND;
    }
}

/////////////
/// CACHE ///
/////////////

int num_cached = 0;

// Least/most-recently used.
cnode * cnode_first;
cnode * cnode_last;

cnode * cnode_root_keys;
cnode * cnode_root_ids;

// Add node as most-recently used.
void
cnode_push_mru (cnode *cn)
{
    cn->prev = NULL;
    cn->next = cnode_first;
    cnode_first = cn;
    if (!cnode_last)
        cnode_last = cn;
}

// Pop least-recently used.
cnode *
cnode_pop_lru ()
{
    cnode * cn = cnode_last;
    if (cn)
        cnode_last = cn->prev;
    return cn;
}

void
cnode_remove_lru (cnode *cn)
{
    if (cn->prev)
        cn->prev->next = cn->next;
    else
        cnode_first = cn->next;
    if (cn->next)
        cn->next->prev = cn->prev;
    if (cn == cnode_last)
        cnode_last = cn->prev;
}

void
cnode_make_mru (cnode *cn)
{
    cnode_remove_lru (cn);
    cnode_push_mru (cn);
}

void
cnode_remove (cnode *cn)
{
    cnode_remove_lru (cn);
    //cnode_remove_id (cn);
    //cnode_remove_key (cn);
    free (cn);
}

cnode *
cnode_alloc (bdb *db, dbid_t id, void *data, size_t size)
{
    cnode *cn;

    // When out of cache...
    if (num_cached > 256) {
        // ...get least-recently used cnode...
        cn = cnode_pop_lru (db);

        // Insert into b-tree if new on storage.
        if (!(cn->flags & CNODE_HAS_BNODE))
            bnode_insert_key (db, db->data2key (cn->data), cn->id);

        // Init/update record data.
        bnode_write_data (db, cn->id, cn->data, cn->size);

        // Remove record from cache.
        cnode_remove (cn);
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
    cnode_push_mru (cn);
    return cn;
}

dbid_t
cnode_idhash (dbid_t id)
{
    // TODO: Divide signed to filter alignment and
    // reverse bit order to ensure a balanced tree. (smk)
    return id;
}

void
cnode_insert_id (bdb *db, cnode *cn)
{
    dbid_t  key = cnode_idhash (cn->id);
    cnode   *n = cnode_root_ids;
    cnode   *on;

    if (!n) {
        cnode_root_ids = cn;
        return;
    }
    for (;;) {
        on = cn;
        if (cnode_idhash (n->id) < key) {
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
cnode_insert_key (bdb *db, cnode *cn)
{
    void    *key = db->data2key (cn->data);
    cnode   *n = cnode_root_keys;
    cnode   *on;

    if (!n) {
        cnode_root_keys = cn;
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
cnode_find_id (bdb *db, dbid_t id)
{
    dbid_t  key = cnode_idhash (id);
    cnode   *n = cnode_root_ids;

    if (!n)
        return NULL;
    for (;;) {
        if (n->id == id)
            return n;
        if (cnode_idhash (n->id) < key) {
            if (!(n = n->ileft))
                return NULL;
        } else
            if (!(n = n->iright))
                return NULL;
    }
}

cnode *
cnode_find_key (bdb *db, void *key)
{
    cnode *n = cnode_root_keys;
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

cnode *
cnode_map (bdb *db, dbid_t id)
{
    bnode  *bn;
    cnode  *cn;
    size_t size;

    // Map from secondary storage.
    if (!(bn = bnode_map (&size, db, id)))
        return NULL;

    // Allocate a new cnode + data memory.
    cn = cnode_alloc (db, id, &bn->data, size);

    // Insert cnode into b-trees.
    cnode_insert_key (db, cn);
    cnode_insert_id (db, cn);

    return cn;
}

///////////
/// API ///
///////////

dbid_t
bdb_add (bdb *db, void *key, void *data, size_t size)
{
    // Allocate ID (and space on storage).
    dbid_t id = bnode_alloc_id (db, size);

    // Add record to cache.
    cnode *cn = cnode_alloc (db, id, data, size);

    // Index cached record.
    cnode_insert_key (db, cn);

    return id;
}

dbid_t
bdb_find (bdb *db, void *key)
{
    cnode * cn = cnode_find_key (db, key);
    return cn ?
        cn->id :
        bnode_find (db, key);
}

// Map record to memory.
void *
bdb_map (bdb *db, int id)
{
    // Search for ID in cache.
    cnode *cn = cnode_find_id (db, id);
    if (!cn)
        // Map in from storage.
        cn = cnode_map (db, id);
    else
        // Make most-recently used.
        cnode_make_mru (cn);

    // Return record pointer.
    return cn->data;
}
