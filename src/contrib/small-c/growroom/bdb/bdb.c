#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <stdio.h>

#include "bdb.h"

#ifdef CACHE
cnode *
cnode_alloc (bdb *db)
{
    (void) db;
    return (void *) ERROR;
}
#endif // #ifdef CACHE

// Write head of bnode only.
void
bdb_write_bnode (bdb *db, dbid_t id, bnode * n)
{
    size_t nwritten = db->write (db, id + sizeof (size_t), n, sizeof (bnode));
    if (nwritten != sizeof (bnode))
        perror ("Error writing data.");
}

// Allocate record on storage.
dbid_t
bdb_alloc (bdb *db, void * data, size_t size)
{
    dbid_t id    = db->next_free;
    size_t nsize = sizeof (bnode) + size;
    size_t nwritten;

    // Allocate cleared memory for bnode and following data.
    bnode *n = malloc (nsize);
    bzero (n, nsize);

    // Copy record data after bnode.
    memcpy (&n->data, data, size);

    // Write size of bnode + data.
    nwritten = db->write (db, id, &nsize, sizeof (size_t));
    if (nwritten != sizeof (size_t))
        perror ("Error writing size.");

    // Write data.
    nwritten = db->write (db, id + sizeof (size_t), n, nsize);
    if (nwritten != nsize)
        perror ("Error writing data.");

    // Step to next free space.
    db->next_free += nsize + sizeof (size_t);
    return id;
}

bnode *
bdb_map_node (bdb *db, int id)
{
    bnode * n;
    size_t nsize;
    size_t nread;

    // Read size of node.
    nread = db->read (db, id, &nsize, sizeof (size_t));
    if (!id && !nread)
        return NULL;  // Missing root node.

    // Allocate memory for node & data.
    if (!(n = malloc (nsize)))
        return NULL;

    // Read node & data.
    nread = db->read (db, id + sizeof (size_t), n, nsize);
    if (nread != nsize)
        perror ("bdb_map_node(): Reading data failed.");

    return n;
}

void *
bdb_map (bdb *db, int id)
{
    bnode *n = bdb_map_node (db, id);
    if (n)
        return &n->data;
    return NULL;
}

int
bdb_add (bdb *db, void *key, void *v, size_t size)
{
    bnode *n;
    dbid_t id = 0;
    dbid_t oid = 0;
    for (;;) {
        if (!(n = bdb_map_node (db, id)))
            return bdb_alloc (db, v, size);
        oid = id;
        if (db->test (db, &n->data, key) < 0) {
            if (!(id = n->left)) {
                n->left = bdb_alloc (db, v, size);
                bdb_write_bnode (db, oid, n);
                return n->left;
            }
        } else
            if (!(id = n->right)) {
                n->right = bdb_alloc (db, v, size);
                bdb_write_bnode (db, oid, n);
                return n->right;
            }
    }
}

dbid_t
bdb_find (bdb *db, void *key)
{
    bnode *n;
    dbid_t id = 0;
    int c;
    for (;;) {
        if (!(n = bdb_map_node (db, id)))
            perror ("Internal error. Node not mapped.");
        if (!(c = db->test (db, &n->data, key)))
            return id;
        if (c < 0) {
            if (!(id = n->left))
                return NOTFOUND;
        } else
            if (!(id = n->right))
                return NOTFOUND;
    }
}

int
bdb_remove (bdb *db, dbid_t id)
{
    (void) db;
    (void) id;
    // Submit to your phantasy.
    return -1;
}
