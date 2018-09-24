/*
 * Ultimem file system
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ultifs.h"

#define ULTIFS_START    0x10000
#define EMPTY_PTR       ((upos) -1)

#ifndef __CC65__
    #include <sys/types.h>
    #include <unistd.h>
    #include <wait.h>
    #define STORE_SIZE      (8 * 1024 * 1024)
    unsigned char store[STORE_SIZE];
#else
    #include <cbm.h>
    #include <ultimem-basics.h>
    unsigned char * store = (void *) 0xa000;
#endif

/*
#ifdef __CC65__

char * __cc65fastcall__
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

char ** __cc65fastcall__
split_pathname (char * pathname)
{
    char * pn = strdup (pathname);
    char ** arr = malloc (sizeof (char *) * 8);
    unsigned char n = 0;

    bzero (arr, sizeof (char *) * 8);
    while ((arr[n++] = strdup (strsep (&pn, ","))));

    free (pn);
    return arr;
}

void __cc65fastcall__
free_pathname (char ** arr)
{
    unsigned char n = 0;

    while (arr[n])
        free (arr[n++]);
    free (arr);
}
*/

/*
 * File header
 *
 * The one and only data structure keeping the file system together.
 */

#define BLOCKTYPE_FILE       0xfe
#define BLOCKTYPE_DIRECTORY  0xfd

struct _block {
    usize   size;           /* Size of file data. */
    upos    replacement;    /* Replacement if this file or EMPTY_PTR. */
    upos    next;           /* Next file in directory or EMPTY_PTR. Not valid if replaced. */
    char    type;
    char    name_length;
    /* name */
    /* file data */
#ifndef __CC65__
} __attribute__((packed));
#else
};
#endif
typedef struct _block block;


/*
 * ULTIMEM ACCESS
 */

#ifndef __CC65__

unsigned char
ultimem_read_byte (upos p)
{
    return store[p];
}

void
ultimem_write_byte (upos p, unsigned char v)
{
    store[p] = v & 0xff;
}

#else

unsigned char __cc65fastcall__
ultimem_read_byte (upos p)
{
    unsigned char bank = p >> 13;
    unsigned char addr = (p & 0x1fff) | 0xa000;
    unsigned char oldbank = *ULTIMEM_BLK5RAM;
    unsigned char v;

    *ULTIMEM_BLK5RAM = bank;
    v = *(unsigned char *) p;
    *ULTIMEM_BLK5RAM = oldbank;

    return v;
}

void __cc65fastcall__
ultimem_write_byte (upos p, unsigned char v)
{
    unsigned char bank = p >> 13;
    unsigned char addr = (p & 0x1fff) | 0xa000;
    unsigned char oldbank = *ULTIMEM_BLK5RAM;

    *ULTIMEM_BLK5RAM = bank;
    *(unsigned char *) p = v;
    *ULTIMEM_BLK5RAM = oldbank;
}

#endif

upos __cc65fastcall__
ultimem_read_int (upos p)
{
    return ultimem_read_byte (p)
               | ((upos) ultimem_read_byte (p + 1)) << 8
               | ((upos) ultimem_read_byte (p + 2)) << 16
               | ((upos) ultimem_read_byte (p + 3)) << 24;
}

void __cc65fastcall__
ultimem_write_int (upos p, upos v)
{
    ultimem_write_byte (p, v);
    ultimem_write_byte (p + 1, (v >> 8));
    ultimem_write_byte (p + 2, (v >> 16));
    ultimem_write_byte (p + 3, (v >> 24));
}

void __cc65fastcall__
ultimem_readm (char * dest, char len, upos p)
{
    while (len--)
        *dest++ = ultimem_read_byte (p);
}


/*
 * BLOCK FUNCTIONS
 */

unsigned char __cc65fastcall__
block_get_type (upos p)
{
    return ultimem_read_byte (p + offsetof (block, type));
}

unsigned char __cc65fastcall__
block_get_name_length (upos p)
{
    return ultimem_read_byte (p + offsetof (block, name_length));
}

unsigned char __cc65fastcall__
block_get_name (upos p, unsigned char i)
{
    return ultimem_read_byte (p + offsetof (block, name_length) + 1 + i);
}

upos __cc65fastcall__
block_get_replacement (upos p)
{
    return ultimem_read_int (p + offsetof (block, replacement));
}

upos __cc65fastcall__
block_get_next (upos p)
{
    return ultimem_read_int (p + offsetof (block, next));
}

usize __cc65fastcall__
block_get_size (upos p)
{
    return ultimem_read_int (p + offsetof (block, size));
}

void __cc65fastcall__
block_set_size (upos p, usize size)
{
    ultimem_write_int (p + offsetof (block, size), size);
}

void __cc65fastcall__
block_set_next (upos p, upos next)
{
    ultimem_write_int (p + offsetof (block, next), next);
}

void __cc65fastcall__
block_set_replacement (upos p, upos replacement)
{
    ultimem_write_int (p + offsetof (block, replacement), replacement);
}

void __cc65fastcall__
block_set_type (upos p, char type)
{
    ultimem_write_byte (p + offsetof (block, type), type);
}

void __cc65fastcall__
block_set_name_length (upos p, char name_length)
{
    ultimem_write_byte (p + offsetof (block, name_length), name_length);
}

usize __cc65fastcall__
block_header_size (upos p)
{
    return sizeof (block) + block_get_name_length (p);
}

upos __cc65fastcall__
file_data (upos p)
{
    return p + block_header_size (p);
}

upos __cc65fastcall__
block_get_latest_version (upos p)
{
    upos r;

    if (p == EMPTY_PTR)
        return p;

    while (1) {
        r = block_get_replacement (p);
        if (r == EMPTY_PTR)
            break;
        p = r;
    }

    return p;
}

upos __cc65fastcall__
block_directory_get_first (upos parent)
{
    upos d = ultimem_read_int (file_data (parent));

    if (d == EMPTY_PTR)
        return d;
    return block_get_latest_version (d);
}

upos __cc65fastcall__
block_get_last (upos p)
{
    upos n;

    while (1) {
        p = block_get_latest_version (p);
        n = block_get_next (p);
        if (n == EMPTY_PTR)
            break;
        p = n;
    }

    return p;
}


/*
 * API
 */

upos last_free;

bfile * __cc65fastcall__
bfile_open (upos directory, upos p)
{
    bfile * b = calloc (1, sizeof (bfile));

    p = block_get_latest_version (p);
    b->start = p;
    b->ptr = file_data (p);
    b->directory = directory;

    return b;
}

bfile * __cc65fastcall__
bfile_create (upos directory, char * name, char type)
{
    char name_length = strlen (name);
    bfile * b;

    b = calloc (1, sizeof (bfile));
    b->directory = directory;
    b->start = last_free;
    b->size = 0;

    block_set_type (last_free, type);
    block_set_name_length (last_free, name_length);
    last_free += 1 + offsetof (block, name_length);
    while (name_length--)
        ultimem_write_byte (last_free++, *name++);

    b->ptr = last_free;

    return b;
}

bfile * __cc65fastcall__
bfile_replace (bfile * old, upos directory, char * name, char type)
{
    bfile * new = bfile_create (directory, name, type);
    new->replaced = old->start;

    return new;
}

void __cc65fastcall__
bfile_remove (bfile * b)
{
    (void) bfile_replace (b, 0, "", 0); /* bs */
}

void __cc65fastcall__
bfile_write (bfile * b, char byte)
{
    ultimem_write_byte (b->ptr, byte);
    b->ptr++;
    b->size++;
}

void __cc65fastcall__
bfile_writem (bfile * b, char * bytes, usize len)
{
    while (len--)
        bfile_write (b, *bytes++);
}

void __cc65fastcall__
bfile_link_replacement (bfile * new)
{
    upos old = new->replaced;

    block_set_replacement (old, new->start);
    block_set_next (new->start, block_get_next (old));
}

void __cc65fastcall__
bfile_append_to_directory (bfile * b)
{
    upos p = ULTIFS_START;

    if (b->directory) {
        p = block_directory_get_first (b->directory);
        if (p == EMPTY_PTR) {
            /* Create pointer to first file of directory. */
            ultimem_write_int (file_data (b->directory), b->start);
            return;
        }
    }

    p = block_get_last (p);
    if (p != b->start)  /* Avoid circularity in very first file. */
        block_set_next (p, b->start);
}

void __cc65fastcall__
bfile_close (bfile * b)
{
    block_set_size (b->start, b->size);
    last_free = b->ptr;

    /* Connect file to directory tree. */
    if (b->replaced)
        bfile_link_replacement (b);
    else
        bfile_append_to_directory (b);

    free (b);
}

upos __cc65fastcall__
bfile_create_directory (upos parent, char * name)
{
    upos d;

    // Create file with empty pointer to the first file.
    bfile * b = bfile_create (parent, name, BLOCKTYPE_DIRECTORY);
    bfile_write (b, 255);
    bfile_write (b, 255);
    bfile_write (b, 255);
    bfile_write (b, 255);
    d = b->start;
    bfile_close (b);

    return d;
}

#define IS_BFILE_REMOVED(p) (!block_get_size (p))

upos __cc65fastcall__
bfile_lookup_name (upos p, char * name, char namelen)
{
    char * buf = malloc (namelen);

    if (!p)
        p = ULTIFS_START;

    do {
        if (namelen != block_get_name_length (p))
            continue;
        ultimem_readm (buf, namelen, file_data (p));
        if (!memcmp (buf, name, namelen))
            break;
    } while (EMPTY_PTR != (p = block_get_next (p)));

    free (buf);

    if (p == EMPTY_PTR)
        return 0;
    p = block_get_latest_version (p);
    if (IS_BFILE_REMOVED(p))
        return 0;
    return p;
}

#ifdef __CC65__

upos current_directory;
upos pwd = ULTIFS_START;

char
ultifs_opendir ()
{
    current_directory = pwd;

    return 0;
}

char __fastcall__
ultifs_readdir (struct cbm_dirent * dirent)
{
    char type = 0;
    char name_length;
    char i;

    if (current_directory == EMPTY_PTR)
        return 1;

    switch (block_get_type (current_directory)) {
        case BLOCKTYPE_FILE:
            type = CBM_T_PRG;
            break;

        case BLOCKTYPE_DIRECTORY:
            type = CBM_T_DIR;
            break;
    }
    dirent->type = type;

    name_length = block_get_name_length (current_directory);
    for (i = 0; i < name_length; i++)
        dirent->name[i] = block_get_name (current_directory, i);

    dirent->size = block_get_size (current_directory) >> 8;

    current_directory = block_get_next (current_directory);

    return 0;
}

void
ultifs_closedir ()
{
}

#endif

#ifndef __CC65__

/*
upos
bfile_lookup (char * name)
{
    char ** arr = split_pathname (name);
    upos p = 0;
    unsigned char i = 0;
    char l;
    char * n;

    while ((n = arr[i])) {
        l = strlen (name);
        p = bfile_lookup_name (p, n, l);
        if (!p || !arr[i + 1])
            break;
        i++;
    }

    free_pathname (arr);
    return p;
}
*/

void
mkfs ()
{
    memset (store, 0xff, STORE_SIZE);
    last_free = ULTIFS_START;
    printf ("Created new image.\n");
}

void
load_file (upos dir, char * name, char * pathname)
{
    FILE * f;
    usize size;
    void * data;
    bfile * b;

    if (0) { //dir) {
        if (!fork ())
            execl ("/usr/local/bin/exomizer", "exomizer", "raw", "-o", "tmp.prg", pathname, NULL);
        else
            wait (NULL);
        pathname = "tmp.prg";
    }
    
    f = fopen (pathname, "rb");
    fseek (f, 0, SEEK_END);
    size = ftell (f);
    b = bfile_create (dir, name, BLOCKTYPE_FILE);
    data = malloc (size);
    fseek (f, 0, 0);
    fread (data, size, 1, f);
    fclose (f);
    bfile_writem (b, data, size);
    free (data);
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

    while ((entry = readdir (dir))) {
        char path[1024];
        if (!strcmp (entry->d_name, ".") || !strcmp (entry->d_name, ".."))
            continue;
        snprintf (path, sizeof (path), "%s/%s", name, entry->d_name);
 
        if (entry->d_type == DT_DIR) {
            printf ("%*sImporting directory %s/\n", indent, "", entry->d_name);
            bsubdir = bfile_create_directory (bparent, entry->d_name);
            import_directory (bsubdir, path, indent + 2);
        } else {
            printf ("%*sImporting %s\n", indent, "", entry->d_name);
            load_file (bparent, entry->d_name, path);
        }
    }
    closedir (dir);
}

void
help ()
{
    printf ("UltiFS image manager\n");
    printf ("\n");
    printf ("Usage: ultifs image command [options]\n");
    printf ("\n");
    printf ("Commands:\n");
    printf ("\n");
    printf ("  n             Format image.\n");
    printf ("  l file        Load file to start of image.\n");
    printf ("  i directory   Import directory recursively.\n");
    printf ("  w             Write image.\n");
    exit (255);
}

void
invalid (char * msg)
{
    fprintf (stderr, "Invalid arguments: %s\n", msg);
    exit (255);
}

char * image_name;

void
load (char * pathname)
{
    FILE * f = fopen (pathname, "rb");
    fread (store, 65536, 1, f);
    fclose (f);
    printf ("Loaded boot file '%s'.\n", pathname);
}

void
write_image ()
{
    FILE * img = fopen (image_name, "w");
    fwrite (store, STORE_SIZE, 1, img);
    fclose (img);
}

int
main (int argc, char ** argv)
{
    unsigned i;

    if (argc == 1)
        help ();
    if (argc < 3)
        invalid ("Invalid number of arguments.");
    image_name = argv[1];
    printf ("Image file: %s\n", image_name);
    i = 2;
    do {
        char * command = argv[i++];
        if (strlen (command) != 1)
            invalid ("Command must be a single character.");
        switch (*command) {
            case 'n':
                mkfs ();
                continue;
            case 'l':
                if (i == argc)
                    invalid ("Path of boot file.");
                load (argv[i++]);
                continue;
            case 'i':
                if (i == argc)
                    invalid ("Path of directory to import missing.");
                printf ("Recurisvely importing direcotry '%s'…\n", argv[i]);
                import_directory (0, argv[i++], 0);
                continue;
            case 'w':
                write_image ();
                printf ("Image written.\n");
                continue;
            default:
                invalid ("Unknown command.");
        }
    } while (i < argc);

    return 0;
}

#endif
