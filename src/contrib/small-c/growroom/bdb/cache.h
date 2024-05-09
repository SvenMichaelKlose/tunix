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
// See also: https://en.wikipedia.org/wiki/Binary_search_tree
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
    cnode   *kparent;
    cnode   *kleft;
    cnode   *kright;

    // ID b-tree
    cnode   *iparent;
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

#ifdef TESTS
extern cnode * cnode_alloc (void);
extern void    cache_push_mru (bdb *, cnode *);
extern void    cache_remove_lru (bdb *, cnode *);
extern cnode * cache_pop_lru (bdb *);
extern char    bit_reverse (char);
extern dbid_t  cache_idhash (dbid_t);
extern void    cache_index_remove_id (bdb *, cnode *);
extern void    cache_index_remove_key (bdb *, cnode *);
extern void    cache_remove (bdb *, cnode *);
extern void    cache_swap_out_lru (bdb *);
#endif // #ifdef TESTS

#endif // #ifndef __CACHE_H__
