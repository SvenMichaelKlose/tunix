/*
 * Ultimem Flash ROM file system
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef __CC65__
#define STORE_SIZE      (8 * 1024 * 1024)
#else
#define STORE_SIZE      1
#endif

#define ULTIFS_START    0x400
#define EMPTY_PTR       -1

#ifdef __CC65__

char *
strsep (char ** str, char * delim)
{
    char * p = *str;
    char * n;

    if (!p)
        return NULL;

    n = strtok (p, delim);
    *str = n;
    *--n = 0;

    return n;
}

#endif

char **
split_pathname (char * pathname)
{
    char * pn = strdup (pathname);
    char ** arr = malloc (sizeof (char *) * 8);
    char n = 0;

    bzero (arr, sizeof (char *) * 8);
    while (arr[n++] = strdup (strsep (&pn, ",")));

    free (pn);
    return arr;
}

void
free_pathname (char ** arr)
{
    char n = 0;

    while (arr[n])
        free (arr[n++]);
    free (arr);
}

unsigned char store[STORE_SIZE];

/*
 * ROM autostart header
 */
struct ultifs {
    short cold_start;
    short warm_start;
    char autostart_id[5];
    unsigned version;
};

#ifndef __CC65__
typedef unsigned int upos;
typedef unsigned int usize;
#else
typedef unsigned long upos;
typedef unsigned long usize;
#endif

/*
 * Header of file data
 */
struct _block {
    usize   size;           /* Size of file data. */
    upos    replacement;    /* Replacement if this file or EMPTY_PTR. */
    upos    next;           /* Next file in directory or EMPTY_PTR. Not valid if replaced. */
    char    type;
    char    name_length;
    /* name */
    /* file data */
#ifndef __CC65__
}__attribute__((packed));
#else
};
#endif
typedef struct _block block;

#define BLOCKTYPE_FILE       ~1
#define BLOCKTYPE_DIRECTORY  ~2

/*
 * File access info
 */
struct _bfile {
    upos    start;
    upos    ptr;
    upos    directory;
    upos    replaced;
};
typedef struct _bfile bfile;

upos last_free;

bfile * bfile_create (upos directory, char * name, usize size, char type);
upos bfile_lookup_name (upos p, char * name, char ln);
void bfile_close (bfile * b);

/*
 * ULTIMEM ACCESS
 */

#ifndef __CC65__

unsigned char
ultimem_read_byte (upos p)
{
    return store[p];
}

upos
ultimem_read_int (upos p)
{
    return store[p] | (store[p + 1] << 8) | (store[p + 2] << 16) | (store[p + 3] << 24);
}

void
ultimem_write_byte (upos p, unsigned char v)
{
    store[p] = v & 0xff;
}

void
ultimem_write_int (upos p, upos v)
{
    store[p] = v & 0xff;
    store[p + 1] = (v >> 8) & 0xff;
    store[p + 2] = (v >> 16) & 0xff;
    store[p + 3] = (v >> 24) & 0xff;
}

#else

unsigned char
ultimem_read_byte (upos p)
{
    return 0;
}

upos
ultimem_read_int (upos p)
{
    return 0;
}

void
ultimem_write_byte (upos p, unsigned char v)
{
}

void
ultimem_write_int (upos p, upos v)
{
}

#endif

void
ultimem_readm (char * dest, char len, upos p)
{
    while (len--)
        *dest++ = ultimem_read_byte (p);
}

/*
 * BFILE FUNCTIONS
 */

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

upos
bfile_get_replacement (upos p)
{
    upos r;

    if (p == EMPTY_PTR)
        return p;

    while (1) {
        r = ultimem_read_int (p + offsetof (block, replacement));
        if (r == EMPTY_PTR)
            break;
        p = r;
    }

    return p;
}

bfile *
bfile_open (upos p)
{
    bfile * b = calloc (1, sizeof (bfile));

    p = bfile_get_replacement (p);
    b->start = p;
    b->ptr = file_data (p);

    return b;
}

bfile *
bfile_replace (bfile * old, upos directory, char * name, usize size, char type)
{
    bfile * new = bfile_create (directory, name, size, type);
    new->replaced = old->start;

    return new;
}


bfile *
bfile_create (upos directory, char * name, usize size, char type)
{
    char l = strlen (name);
    bfile * b;

    b = calloc (1, sizeof (bfile));
    b->directory = directory;
    b->start = last_free;

    ultimem_write_int (last_free + offsetof (block, size), size);
    ultimem_write_byte (last_free + offsetof (block, type), type);
    ultimem_write_byte (last_free + offsetof (block, name_length), l);
    last_free += 1 + offsetof (block, name_length);
    while (l--)
        ultimem_write_byte (last_free++, *name++);

    b->ptr = last_free;
    last_free += size;

    return b;
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
bfile_writem (bfile * b, char * bytes, usize len)
{
    while (len--)
        bfile_write (b, *bytes++);
}

void
bfile_link_replacement (bfile * new)
{
    upos old = new->replaced;

    ultimem_write_int (old + offsetof (block, replacement), new->start);
    ultimem_write_int (new->start + offsetof (block, next), ultimem_read_int (old + offsetof (block, next)));
}

upos
directory_first (upos p)
{
    upos d = ultimem_read_int (file_data (p));

    if (d == EMPTY_PTR)
        return d;
    return bfile_get_replacement (d);
}

upos
directory_last (upos p)
{
    upos n;

    while (1) {
        p = bfile_get_replacement (p);
        n = ultimem_read_int (p + offsetof (block, next));
        if (n == EMPTY_PTR)
            break;
        p = n;
    }

    return p;
}

void
bfile_append_to_directory (bfile * b)
{
    upos p = ULTIFS_START;

    if (b->directory) {
        p = directory_first (b->directory);
        if (p == EMPTY_PTR) {
            /* Create pointer to first file of directory. */
            ultimem_write_int (file_data (b->directory), b->start);
            return;
        }
    }

    p = directory_last (p);
    if (p != b->start)  /* Avoid circularity in very first file. */
        ultimem_write_int (p + offsetof (block, next), b->start);
}

void
bfile_close (bfile * b)
{
    /* Connect file to directory tree. */
    if (b->replaced)
        bfile_link_replacement (b);
    else
        bfile_append_to_directory (b);

    free (b);
}

upos
bfile_create_directory (upos parent, char * name)
{
    upos d;

    // Create file with empty pointer to the first file.
    bfile * b = bfile_create (parent, name, sizeof (upos), BLOCKTYPE_DIRECTORY);
    d = b->start;
    bfile_close (b);

    return d;
}

#define IS_BFILE_REMOVED(p) (!ultimem_read_int (p + offsetof (block, size)))

upos
bfile_lookup_name (upos p, char * name, char ln)
{
    char * buf = malloc (ln);
    char lh;

    if (!p)
        p = ULTIFS_START;

    do {
        lh = ultimem_read_byte (p + offsetof (block, name_length));
        if (ln != lh)
            continue;
        ultimem_readm (buf, ln, file_data (p));
        if (!memcmp (buf, name, ln))
            break;
    } while (EMPTY_PTR != (p = ultimem_read_int (p + offsetof (block, next))));

    free (buf);

    if (p == EMPTY_PTR || IS_BFILE_REMOVED(p))
        return 0;
    return bfile_get_replacement (p);
}

upos
bfile_lookup (char * name)
{
    char ** arr = split_pathname (name);
    upos p = 0;
    char i = 0;
    char l;
    char * n;

    while (n = arr[i]) {
        l = strlen (name);
        p = bfile_lookup_name (p, n, l);
        if (!p || !arr[i + 1])
            break;
        i++;
    }

    free_pathname (arr);
    return p;
}

void
mount ()
{
    upos p = ULTIFS_START;
    usize size;

    while (1) {
        size = ultimem_read_int (p);
        if (size == EMPTY_PTR)
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

void
load_file (upos dir, char * name, char * pathname)
{
    FILE * f = fopen (pathname, "rb");
    usize s;
    void * data;
    bfile * b;

    fseek (f, 0, SEEK_END);
    s = ftell (f);
    b = bfile_create (dir, name, s, BLOCKTYPE_FILE);
    data = malloc (s);
    fseek (f, 0, 0);
    fread (data, s, 1, f);
    fclose (f);
    bfile_writem (b, data, s);
    bfile_close (b);
}

#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>

void import_directory (upos bparent, char * name, int indent)
{
    DIR * dir;
    struct dirent * entry;
    upos bsubdir;

    if (!(dir = opendir (name)))
        return;

    while (entry = readdir (dir)) {
        char path[1024];
        if (!strcmp (entry->d_name, ".") || !strcmp (entry->d_name, "..") || !strcmp (entry->d_name, "fs.img"))
            continue;
        snprintf (path, sizeof (path), "%s/%s", name, entry->d_name);
 
        if (entry->d_type == DT_DIR) {
            printf ("%*s[%s]\n", indent, "", entry->d_name);
            bsubdir = bfile_create_directory (bparent, entry->d_name);
            import_directory (bsubdir, path, indent + 2);
        } else {
            printf ("%*s- %s\n", indent, "", entry->d_name);
            load_file (bparent, entry->d_name, path);
        }
    }
    closedir(dir);
}

int
main (char ** argv, int argc)
{
    bfile * b;
    upos dir;

    memset (store, 0xff, STORE_SIZE);
    mkfs ();
    last_free = ULTIFS_START;

    import_directory (0, ".", 0);

    FILE * img = fopen ("fs.img", "w");
    fwrite (store, STORE_SIZE, 1, img);
    fclose (img);

    return 0;
}

#endif
