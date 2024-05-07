#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <err.h>

#include "bdb.h"
#include "storage.h"

size_t
snode_size (size_t data_size)
{
    return (sizeof (snode) - 1) + data_size;
}

size_t
storage_size (size_t data_size)
{
    return sizeof (size_t) + snode_size (data_size);
}

// Allocate ID (and space) for new record.
dbid_t
storage_alloc_id (bdb *db, size_t size)
{
    dbid_t id = db->next_free;
    db->next_free += storage_size (size);
    return id;
}

// Write size before snode to storage.
bool
storage_write_size (bdb *db, dbid_t id, size_t size)
{
    size_t nsize = snode_size (size);
    size_t nwritten;
    assert (db->write);
    nwritten = db->write (db, id, &nsize, sizeof (size_t));
    return nwritten != sizeof (size_t);
}

// Write head of snode only to storage.
void
storage_write_snode (bdb *db, dbid_t id, snode *n)
{
    size_t nwritten = db->write (db, id + sizeof (size_t), n, snode_size (0));
    if (nwritten != snode_size (0))
        err (EXIT_FAILURE, "Error writing data.");
}

// Write data after snode to storage.
bool
storage_write_data (bdb *db, dbid_t id, void *data, size_t size)
{
    size_t nsize = storage_size (0);
    size_t nwritten = db->write (db, id + storage_size (0), data, size);
    return nwritten != nsize;
}

size_t
storage_read_size (bdb *db, dbid_t id)
{
    size_t nsize;
    size_t nread;
    assert(db->read);
    nread = db->read (db, id, &nsize, sizeof (size_t));
    if (nread != sizeof (size_t))
        err (EXIT_FAILURE, "storage_map(): Error reading record size.");
    return nsize - snode_size (0);
}

void
storage_read_snode_and_data (bdb *db, dbid_t id, snode *n, size_t data_size)
{
    size_t nread = db->read (db, id + sizeof (size_t), n, snode_size (data_size));
    if (nread != snode_size (data_size))
        err (EXIT_FAILURE, "storage_map(): Reading data failed.");
}

snode *
storage_map (size_t *size, bdb *db, dbid_t id)
{
    snode * n;

    // Handle cache-only root node.
    if (id >= db->filled)
        return NULL;

    *size = storage_read_size (db, id);
    n = malloc (snode_size (*size));
    storage_read_snode_and_data (db, id, n, *size);

    return n;
}

// https://en.wikipedia.org/wiki/Binary_search_tree
void
storage_insert_key (bdb *db, void *key, dbid_t recid)
{
    snode   *n;
    dbid_t  id = 0;
    dbid_t  oid = 0;
    size_t  unused_size;

    for (;;) {
        if (!(n = storage_map (&unused_size, db, id)))
            return; // Root node.

        oid = id;

        if (db->compare (db, &n->data, key) < 0) {
            if (!(id = n->left)) {
                n->left = recid;
                storage_write_snode (db, oid, n);
                break;
            }
        } else
            if (!(id = n->right)) {
                n->right = recid;
                storage_write_snode (db, oid, n);
                break;
            }
        free (n);
    }
    free (n);
}

// Add record to storage.
void
storage_add (bdb *db, dbid_t id, void *data, size_t size)
{
    dbid_t next_id = id + storage_size (size);
    snode sn;
    bzero (&sn, snode_size (0));

    storage_write_size  (db, id, size);
    storage_write_snode (db, id, &sn);
    storage_write_data  (db, id, data, size);
    storage_insert_key  (db, db->data2key (data), id);

    // Keep track of storage size.
    if (db->filled < next_id )
        db->filled = id + storage_size (size);
}

dbid_t
storage_find (bdb *db, void *key)
{
    snode  *n;
    dbid_t id = 0;
    int    c;
    size_t unused_size;

    for (;;) {
        if (!(n = storage_map (&unused_size, db, id)))
            break;
        if (!(c = db->compare (db, &n->data, key))) {
            free (n);
            return id;
        }
        if (c < 0) {
            if (!(id = n->left))
                break;
        } else
            if (!(id = n->right))
                break;
    }
    free (n);
    return NOTFOUND;
}
