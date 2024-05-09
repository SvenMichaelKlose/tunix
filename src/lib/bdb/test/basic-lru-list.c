#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "storage.h"
#include "cache.h"

#include "unity.h"

bdb testdb;

void
cache_test_simple_list (void)
{
    cnode *cn  = cnode_alloc ();
    cnode *cn2 = cnode_alloc ();
    bdb *db    = &testdb;

    TEST_ASSERT_MESSAGE(!db->cache_mru, "There should be no first.");
    TEST_ASSERT_MESSAGE(!db->cache_lru, "There should be no last.");

    printf ("Adding first to list.\n");
    cache_push_mru (db, cn);
    TEST_ASSERT_MESSAGE(db->cache_mru == cn, "Not the first in LRU.");
    TEST_ASSERT_MESSAGE(db->cache_lru == cn, "Not the last in LRU.");
    TEST_ASSERT_MESSAGE(!cn->next, "First must not have a next.");
    TEST_ASSERT_MESSAGE(!cn->prev, "First must not have a prev.");

    printf ("Adding second to list.\n");
    cache_push_mru (db, cn2);
    TEST_ASSERT_MESSAGE(db->cache_mru == cn2, "Not the first in LRU.");
    TEST_ASSERT_MESSAGE(db->cache_lru != cn2, "Must not be the last in LRU.");
    TEST_ASSERT_MESSAGE(!cn->next, "First must not have a next.");
    TEST_ASSERT_MESSAGE(cn->prev == cn2, "First must point back to second.");
    TEST_ASSERT_MESSAGE(!cn2->prev, "Second must not have a prev.");
    TEST_ASSERT_MESSAGE(cn2->next == cn, "Second must point to first.");

    printf ("Taking LRU from list.\n");
    TEST_ASSERT_MESSAGE(cache_pop_lru (db) == cn, "Not the first record.");

    printf ("Taking LRU from list.\n");
    TEST_ASSERT_MESSAGE(cache_pop_lru (db) == cn2, "Not the second record.");
    TEST_ASSERT_MESSAGE(!db->cache_mru, "There must be no first any more.");
    TEST_ASSERT_MESSAGE(!db->cache_lru, "There must be no last any more.");

    printf ("# Cache list tests OK.\n\n");
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("test/basic-lru-list.c");
  RUN_TEST(cache_test_simple_list, 20);
  return UnityEnd();
}
