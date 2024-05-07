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
make_int_cnode (int x)
{
    cnode *cn = cnode_alloc ();
    cn->data = malloc (sizeof (int));
    *((int *) cn->data) = x;
    cn->size = sizeof (int);
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

    testdb.compare  = compare;
    testdb.data2key = data2key;
    root = make_int_cnode (1);
    root->id = 0;

    cache_insert_id (db, root);
    TEST_ASSERT_MESSAGE(root == db->cache_root_ids, "New cnode should be root of ID index.");
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
