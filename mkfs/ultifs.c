#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __CC65__
#include "ultimem.h"
#endif

#define STORE_SIZE      (8 * 1024 * 1024)
#define ULTIFS_START    0x1800

#ifndef __CC65__
char store[STORE_SIZE];
#endif

typedef unsigned long upos;
typedef unsigned long usize;
typedef unsigned short uid;

upos last_free;

struct _block {
    usize   size;
    char    type;
    upos    replacement;
    upos    next;           /* Next file in directory. */
    char    name_length;
};

typedef struct _block block;

struct _bfile {
    upos    start;
    upos    ptr;
    upos    directory;
    upos    replaced;
};

typedef struct _bfile bfile;

struct ultifs {
    short cold_start;
    short warm_start;
    char autostart_id[5];
    unsigned version;
};

unsigned char
ultimem_read_byte (upos p)
{
    return 0;
}

unsigned short
ultimem_read_word (upos p)
{
    return 0;
}

unsigned long
ultimem_read_long (upos p)
{
    return 0;
}

void
ultimem_write_byte (upos p, unsigned char v)
{
    return;
}

#ifndef __CC65__
#endif

void
ultimem_write_long (upos p, unsigned long v)
{
    return;
}

usize
block_header_size (upos p)
{
    return sizeof (block) + ultimem_read_byte (p + offsetof (block, name_length));
}

upos
file_data (upos p)
{
    return p + block_header_size (p);
}

bfile *
bfile_open (upos p)
{
    bfile * b = malloc (sizeof (bfile));
    upos r;

    while (1) {
        r = ultimem_read_long (p + offsetof (block, replacement));
        if (r == - 1)
            break;
        p = r;
    }
    b->ptr = file_data (p);

    return b;
}

bfile *
bfile_create (upos directory, char * pathname, usize size, char type)
{
    char l = strlen (pathname);
    bfile * b = malloc (sizeof (bfile));
    b->directory = directory;

    ultimem_write_long (last_free + offsetof (block, size), size);
    ultimem_write_long (last_free + offsetof (block, type), type);
    ultimem_write_byte (last_free + offsetof (block, name_length), l);
    last_free += 1 + offsetof (block, name_length);
    while (l--)
        ultimem_write_byte (last_free++, *pathname++);

    b->ptr = last_free;
    last_free += size;

    return b;
}

bfile *
bfile_replace (bfile * old, upos directory, char * pathname, usize size, char type)
{
    bfile * new = bfile_create (directory, pathname, size, type);
    new->replaced = old->start;

    return new;
}

void
bfile_remove (bfile * b)
{
    (void) bfile_replace (b, 0, "", 0, 0);
}

void
bfile_write (bfile * b, char byte)
{
    ultimem_write_byte (b->ptr, byte);
    b->ptr++;
}

void
bfile_link_replacement (bfile * new)
{
    upos old = new->replaced;

    ultimem_write_long (old + offsetof (block, replacement), new->start);
    ultimem_write_long (new->start + offsetof (block, next), ultimem_read_long (old + offsetof (block, next)));
}

upos
directory_first (upos p)
{
    return ultimem_read_long (file_data (p));
}

void
bfile_append_to_directory (bfile * b)
{
    upos p = b->directory ? directory_first (b->directory) : ULTIFS_START;
    upos n;

    while (1) {
        n = ultimem_read_long (p + offsetof (block, next));
        if (!n)
            break;
        p = n;
    }

    ultimem_write_long (p + offsetof (block, next), b->start);
}

void
bfile_close (bfile * b)
{
    if (b->replaced)
        bfile_link_replacement (b);
    else
        bfile_append_to_directory (b);

    free (b);
}

void
mount ()
{
    upos p = ULTIFS_START;
    usize size;

    while (1) {
        size = ultimem_read_long (p);
        if (size == -1)
            break;
        p += size + block_header_size (p);
    }
}

#ifndef __CC65__

void
mkfs ()
{
    struct ultifs * ultifs = (void *) store;
    bzero (ultifs, sizeof (struct ultifs));
    strcpy (ultifs->autostart_id, "A0\xc3\xc2\xcd");
}


int
main (char ** argv, int argc)
{
    memset (store, 0xff, STORE_SIZE);
    mkfs ();
    last_free = ULTIFS_START;

    FILE * img = fopen ("fs.img", "w");
    fwrite (store, STORE_SIZE, 1, img);
    fclose (img);

    return 0;
}

#endif
