#include <stdio.h>
#include <string.h>

#define STORE_SIZE  (8 * 1024 * 1024)

char store[STORE_SIZE];

typedef long position;

#define BLKTYPE_ALLOCATED   1
#define BLKTYPE_BLOCKLIST   255

struct block {
    char        type;
    position    start;
    long        size;
};

struct dirent {
    char        type;
    char        name[16];
};

struct ultifs {
    short cold_start;
    short warm_start;
    char autostart_id[5];
    unsigned version;
    position    blocklist;
    long        blocklist_size;
};

#define BANKPTR(x) ((void *) &store[x])
#define PTRBANK(bank_start, ptr) ((position) ptr - (position) bank_start)

/*
 * Allocate block.
 */
position
file_alloc (struct ultifs * ultifs, long size)
{
    struct block * blk = BANKPTR(ultifs->blocklist);
    while (blk->type) {
        if (blk->type == BLKTYPE_BLOCKLIST) {
            if (!blk->start)
                return 0;
            blk = BANKPTR(blk->start);
            continue;
        }
        blk++;
    }
    blk->type = BLKTYPE_ALLOCATED;
    blk->size = size;
    return PTRBANK(ultifs->blocklist, blk);
}

void
make_header ()
{
    struct ultifs * ultifs = (void *) store;
    strcpy (ultifs->autostart_id, "A0\xc3\xc2\xcd");
}

int
main (char ** argv, int argc)
{
    make_header ();
    FILE * img = fopen ("fs.img", "w");
    fwrite (&store, STORE_SIZE, 1, img);
    fclose (img);
    return 0;
}
