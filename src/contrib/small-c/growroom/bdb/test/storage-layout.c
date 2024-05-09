// TODO: Check .bdb file contents. (smk)

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bdb.h"
#include "storage.h"

#include "unity.h"

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

bdb testdb = {
    .compare  = compare,
    .data2key = data2key,
    .read     = bdb_file_read,
    .write    = bdb_file_write,
    .storage  = "storage-layout.bdb"
};

dbid_t
add_int_snode (bdb *db, int x)
{
    dbid_t id = storage_alloc_id (db, sizeof (int));
    storage_add (&testdb, id, &x, sizeof (int));
    return id;
}

void
test_snode_inserts (void)
{
    dbid_t id_root;
    dbid_t id1;

    id_root = add_int_snode (&testdb, 0xdead);
    TEST_ASSERT(id_root == 0);
    id1 = add_int_snode (&testdb, 0xbeef);
    TEST_ASSERT(id1 == ((sizeof (size_t) + snode_size (sizeof (int)))));
}

void setUp (void) {}
void tearDown (void) {}

int
main (void)
{
  UnityBegin("storage-layout.c");
  RUN_TEST(test_snode_inserts);
  return UnityEnd();
}
