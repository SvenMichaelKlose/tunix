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

dbid_t
bdb_alloc (bdb *db, void * data, size_t size)
{
    dbid_t id    = db->next_free;
    size_t nsize = sizeof (bnode) + size - 1;
    bnode *n     = malloc (nsize);
    n->left = n->right = 0;
    memcpy (&n->data, data, size);
    db->write (db, id, &nsize, sizeof (size_t));
    db->write (db, id + sizeof (size_t), n, nsize);
    db->next_free += nsize;
    return id;
}

void *
bdb_map (bdb *db, int id)
{
    bnode * n;
    size_t nsize;
    db->read (db, id, &nsize, sizeof (size_t));
    if (!(n = malloc (nsize)))
        return (void *) ERROR;
    db->read (db, id + 2, n, nsize);
    return &n->data;
}

int
bdb_add (bdb *db, void *key, void *v, size_t size)
{
    bnode *p;
    dbid_t id = 0;
    for (;;) {
        p = bdb_map (db, id);
        if (db->test (&p->data, key) < 0) {
            if (!(id = p->left))
                return p->left =
                    bdb_alloc (db, v, size);
        } else
            if (!(id = p->right))
                return p->right =
                    bdb_alloc (db, v, size);
        free (p);
    }
}

dbid_t
bdb_lookup (bdb *db, void *key)
{
    bnode *p;
    dbid_t id = 0;
    int c;
    for (;;) {
        p = bdb_map (db, id);
        c = db->test (&p->data, key);
        if (!c)
            return id;
        if (c < 0) {
            if (!(id = p->left))
                return NOTFOUND;
        } else
            if (!(id = p->right))
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
