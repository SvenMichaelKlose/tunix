#ifndef __BDB_H__
#define __BDB_H__

#define ERROR       -1
#define NOTFOUND    -1

typedef unsigned int dbid_t;

typedef struct _bnode {
    dbid_t  left;
    dbid_t  right;
    char    data[1];
} bnode;

#ifdef CACHE
typedef struct _cnode {
    int  id;
    // Deque
    int  next;
    int  prev;
    char data[1];
} cnode;
#endif // #ifdef CACHE

typedef struct _bdb bdb;
typedef struct _bdb {
    dbid_t next_free;
    int (*test)  (bdb *db, void *rec, void *key);
    int (*read)  (bdb *db, dbid_t ofs, void *r, size_t size);
    int (*write) (bdb *db, dbid_t ofs, void *r, size_t size);
} bdb;

dbid_t  bdb_alloc (bdb *db, void *data, size_t size);
void *  bdb_map (bdb *db, int id);
int     bdb_add (bdb *db, void *key, void *data, size_t size);
dbid_t  bdb_find (bdb *db, void *key);
int     bdb_remove (bdb *db, dbid_t id);

#endif // #ifndef __BDB_H__
