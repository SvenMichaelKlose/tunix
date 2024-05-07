#ifndef __BDB_H__
#define __BDB_H__

#ifndef BDB_MAX_CACHED
#define BDB_MAX_CACHED 255
#endif

#define ERROR       -1
#define NOTFOUND    -1

typedef unsigned int dbid_t;

typedef struct _cnode cnode;
typedef struct _bdb bdb;
typedef struct _bdb {
    char     flags;
    #define HAS_OPEN_STORAGE    1

    // Storage allocation (growing).
    void     *storage;  // E.g. (FILE *).
    dbid_t   next_free;
    dbid_t   filled;

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

    cnode * cache_mru;
    cnode * cache_lru;
    unsigned num_cached;

    // Cache b-tree roots.
    cnode * cache_root_keys;
    cnode * cache_root_ids;
} bdb;

dbid_t  bdb_add   (bdb *, void *key, void *data, size_t);
dbid_t  bdb_find  (bdb *, void *key);
void *  bdb_map   (bdb *, dbid_t);
void    bdb_close (bdb *);

extern int bdb_file_write (bdb *, dbid_t file_offset, void *data, size_t);
extern int bdb_file_read  (bdb *, dbid_t file_offset, void *data, size_t);

#endif // #ifndef __BDB_H__
