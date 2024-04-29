#ifndef __CACHE_H__
#define __CACHE_H__

typedef struct _cnode cnode;
typedef struct _cnode {
    // Offset on secondary storage.
    dbid_t  id;
    size_t  size;
    #define CNODE_HAS_BNODE     1
    char    flags;

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

extern cnode * cache_alloc      (bdb *, dbid_t, void *data, size_t);
extern void    cache_insert_id  (bdb *, cnode *);
extern void    cache_insert_key (bdb *, cnode *);
extern cnode * cache_find_id    (bdb *, dbid_t);
extern cnode * cache_find_key   (bdb *, void *key);
extern cnode * cache_map        (bdb *, dbid_t);
extern void    cache_make_mru   (cnode *);

/*
extern void cache_push_mru (cnode *cn);
cache_pop_lru ();
cache_remove_lru (cnode *cn);
cache_remove (cnode *cn);
cache_idhash (dbid_t id);
cache_insert_id (bdb *db, cnode *cn);
*/

#endif // #ifndef __CACHE_H__
