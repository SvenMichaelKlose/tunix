#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "storage.h"
#include "cache.h"

#include "unity.h"

bdb testdb;

cnode *
make_int_cnode (int x, dbid_t id)
{
    cnode *cn = cnode_alloc ();
    cn->data = malloc (sizeof (int));
    *((int *) cn->data) = x;
    cn->size = sizeof (int);
    cn->id = id;
    return cn;
}

int
compare (bdb *db, void *rec, void *key)
{
    (void) db;
    int a = *(int *) rec;
    int b = *(int *) key;
    if (a == b)
        return 0;
    if (a < b)
        return -1;
    return 1;
}

void *
data2key (void *rec)
{
    return rec;
}

void
test_cnode_inserts (void)
{
    bdb   *db;
    cnode *root;
    cnode *child1;
    cnode *child2;

    testdb.compare  = compare;
    testdb.data2key = data2key;
    db = &testdb;

    root = make_int_cnode (1, 0);
    cache_insert_id (db, root);
    TEST_ASSERT(root == db->cache_root_ids);

    child1 = make_int_cnode (2, 1);
    cache_insert_id (db, child1);
    TEST_ASSERT(root->iright == NULL);
    TEST_ASSERT(root->ileft == child1);

    child2 = make_int_cnode (3, 2);
    cache_insert_id (db, child2);
    TEST_ASSERT(child1->iright == NULL);
    TEST_ASSERT(child1->ileft == child2);

    cache_index_remove_id (db, child1);
    TEST_ASSERT(db->cache_root_ids);
    TEST_ASSERT(db->cache_root_ids == root);
    TEST_ASSERT(root->iright == NULL);
    TEST_ASSERT(root->ileft == child2);
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("test/cnode-add.c");
  RUN_TEST(test_cnode_inserts, 0);
  return UnityEnd();
}
