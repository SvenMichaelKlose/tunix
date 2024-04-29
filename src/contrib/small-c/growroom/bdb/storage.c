#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "bdb.h"
#include "storage.h"

size_t
snode_size (size_t data_size)
{
    // Add header (- 1 char of data declaration)
    // and record size.
    return sizeof (snode) + data_size - 1;
}

// Allocate ID (and space) for new record.
dbid_t
storage_alloc_id (bdb *db, size_t size)
{
    // An ID is basically an offset into the storage.
    dbid_t id = db->next_free;

    // Make space for size, snode and data.
    db->next_free += snode_size (size) + sizeof (size_t);
    return id;
}

// Write size before snode to storage.
bool
storage_write_size (bdb *db, dbid_t id, size_t size)
{
    size_t nsize = snode_size (size);
    size_t nwritten = db->write (db, id, &nsize, sizeof (size_t));
    return nwritten != sizeof (size_t);
}

// Write head of snode only to storage.
void
storage_write_snode (bdb *db, dbid_t id, snode * n)
{
    size_t nwritten = db->write (db, id + sizeof (size_t), n, snode_size (0));
    if (nwritten != snode_size (0))
        perror ("Error writing data.");
}

// Write data after snode to storage.
bool
storage_write_data (bdb *db, dbid_t id, void * data, size_t size)
{
    size_t nsize = sizeof (size_t) + snode_size (0);
    size_t nwritten = db->write (db, nsize, data, size);
    return nwritten != nsize;
}

// Allocate new record on storage.
dbid_t
storage_alloc (bdb *db, void * data, size_t size)
{
    dbid_t id = storage_alloc_id (db, size);
    if (!storage_write_size (db, id, size))
        perror ("Error writing snode size.");
    if (!storage_write_data (db, id, data, size))
        perror ("Error writing snode data.");
    return id;
}

void *
storage_map (size_t *size, bdb *db, dbid_t id)
{
    snode * n;
    size_t nsize;
    size_t nread;

    // Read size of node.
    nread = db->read (db, id, &nsize, sizeof (size_t));
    if (!id && !nread) {
        printf ("storage_map(): No root node.\n");
        return NULL;  // Missing root node.
    }
    *size = nsize - snode_size (0);

    // Allocate memory for node & data.
    if (!(n = malloc (nsize)))
        return NULL;

    // Read node & data.
    nread = db->read (db, id + sizeof (size_t), n, nsize);
    if (nread != nsize)
        perror ("storage_map(): Reading data failed.");

    return n;
}

void
storage_insert_key (bdb *db, void *key, dbid_t recid)
{
    snode   *n;
    dbid_t  id = 0;
    dbid_t  oid = 0;
    size_t  unused_size;

    for (;;) {
        // Map node into memory.
        if (!(n = storage_map (&unused_size, db, id)))
            return; // Root node.

        // Decide which to child to travel.
        oid = id;  // Save for update.
        if (db->compare (db, &n->data, key) < 0) {
            // Go left.
            if (!(id = n->left)) {
                // Set left child.
                n->left = recid;
                storage_write_snode (db, oid, n);
                return;
            }
        } else
            // Go right.
            if (!(id = n->right)) {
                // Set right child.
                n->right = recid;
                storage_write_snode (db, oid, n);
                return;
            }
    }
}

dbid_t
storage_add (bdb *db, void *key, void *data, size_t size)
{
    dbid_t id = storage_alloc (db, data, size);
    storage_insert_key (db, key, id);
    return id;
}

dbid_t
storage_find (bdb *db, void *key)
{
    snode *n;
    dbid_t id = 0;
    int c;
    size_t unused_size;

    for (;;) {
        // Map node into memory.
        if (!(n = storage_map (&unused_size, db, id)))
            return NOTFOUND;

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
