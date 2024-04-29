#ifndef __BDB_H__
#define __BDB_H__

#define ERROR       -1
#define NOTFOUND    -1

typedef unsigned int dbid_t;

typedef struct _bdb bdb;
typedef struct _bdb {
    dbid_t next_free;

    // Compare key in 'rec' to 'key'.
    // Return 0: match, < 0; "less than", > 0 "greater than"
    // (Use strcmp()/strncmp() on string keys.)
    int  (*compare)  (bdb *db, void *rec, void *key);

    // Plain read from storage.
    int  (*read)     (bdb *db, dbid_t ofs, void *rec, size_t size);

    // Plain write to storage.
    int  (*write)    (bdb *db, dbid_t ofs, void *rec, size_t size);

    // Return key in record.
    void *(*data2key) (void *rec);
} bdb;

dbid_t  bdb_alloc (bdb *db, void *data, size_t size);
void *  bdb_map   (bdb *db, int id);
dbid_t  bdb_add   (bdb *db, void *key, void *data, size_t size);
dbid_t  bdb_find  (bdb *db, void *key);

#endif // #ifndef __BDB_H__
