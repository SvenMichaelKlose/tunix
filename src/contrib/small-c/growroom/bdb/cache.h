#ifndef __CACHE_H__
#define __CACHE_H__

// Cached record
//
// Node in all of:
//
//  * LRU list (deque)
//  * ID index btree
//  * key index btree
//
typedef struct _cnode cnode;
typedef struct _cnode {
    // Offset on secondary storage.
    dbid_t  id;
    size_t  size;

    char    flags;
    #define HAS_STORAGE     1
    #define IS_LOCKED       128 // TODO: bdb_lock()

    // LRU deque
    cnode   *next;
    cnode   *prev;

    // Key b-tree
    cnode   *kleft;
    cnode   *kright;

    // ID b-tree
    cnode   *ileft;
    cnode   *iright;

    // malloc()'ed data
    void    *data;
} cnode;

extern cnode * cache_add         (bdb *, dbid_t, void *data, size_t);
extern cnode * cache_add_stored  (bdb *, dbid_t);
extern void    cache_insert_id   (bdb *, cnode *);
extern void    cache_insert_key  (bdb *, cnode *);
extern cnode * cache_find_id     (bdb *, dbid_t);
extern cnode * cache_find_key    (bdb *, void *key);
extern void    cache_make_mru    (bdb *, cnode *);
extern void    cache_flush       (bdb *);

#endif // #ifndef __CACHE_H__
