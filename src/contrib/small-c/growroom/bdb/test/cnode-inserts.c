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
    bdb *db    = &testdb;
    cnode *root;
    cnode *child1;
    cnode *child2;

    printf ("! Adding ID index root node.\n");
    testdb.compare  = compare;
    testdb.data2key = data2key;
    root = make_int_cnode (1, 0);
    cache_insert_id (db, root);
    TEST_ASSERT_MESSAGE(root == db->cache_root_ids,
                        "New cnode not root of ID index.");

    printf ("! Adding first child.\n");
    child1 = make_int_cnode (2, 1);
    cache_insert_id (db, child1);
    TEST_ASSERT_MESSAGE(root->iright == NULL,
                        "Unexpected left child of ID root.");
    TEST_ASSERT_MESSAGE(root->ileft == child1,
                        "First should be left child of ID root.");

    printf ("! Adding second child.\n");
    child2 = make_int_cnode (3, 2);
    cache_insert_id (db, child2);
    TEST_ASSERT_MESSAGE(child1->iright == NULL,
                        "Unexpected right child of first.");
    TEST_ASSERT_MESSAGE(child1->ileft == child2,
                        "Second isn't left child of ID root.");

    printf ("! Removing first child.\n");
    cache_index_remove_id (db, child1);
    TEST_ASSERT_MESSAGE(db->cache_root_ids,
                        "Root node missing.");
    TEST_ASSERT_MESSAGE(db->cache_root_ids == root,
                        "Root node changed.");
    TEST_ASSERT_MESSAGE(root->iright == NULL,
                        "Unexpected left child of ID root.");
    TEST_ASSERT_MESSAGE(root->ileft == child2,
                        "Second isn't left of ID root.");
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
