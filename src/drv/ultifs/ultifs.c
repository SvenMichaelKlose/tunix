//#define COMPRESS_FILE_DATA_WITH_EXOMIZER

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ultifs.h"

#define STORE_SIZE      0x800000u
#define ULTIFS_UNUSED       ((upos) -1)

typedef unsigned char uchar;

#ifndef __CC65__
    #include <sys/types.h>
    #include <unistd.h>
    #include <wait.h>
    uchar store[STORE_SIZE];
    #define cc65register
#else
    #pragma code-name ("ULTIFS")
    #include <cbm.h>
    #include <ultimem/ultimem.h>
    uchar * store = (void *) 0xa000u;
    #define cc65register    register
#endif

char ultifs_error = 0;
uchar current_parent = 0;
upos parents[8];

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
    uchar n = 0;

    memset (arr, 0, sizeof (char *) * 8);
    while ((arr[n++] = strdup (strsep (&pn, ","))));

    free (pn);
    return arr;
}

void __cc65fastcall__
free_pathname (char ** arr)
{
    uchar n = 0;

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

typedef struct _block {
    usize  size;         // Size of file data.
    upos   replacement;  // Replacement or ULTIFS_UNUSED.
    upos   next;         // Next file in directory or ULTIFS_UNUSED.
                         // Unused if there is a 'replacement'.
    char   type;         // CBM file type.
    char   name_length;  // (No terminating 0.)
    /* name */
    /* data */
}
#ifndef __CC65__
    __attribute__((packed))
#endif
    block;


/*
 * ULTIMEM ACCESS
 */

#ifndef __CC65__

#define CBM_T_REG      0x10
#define CBM_T_SEQ      0x10
#define CBM_T_PRG      0x11
#define CBM_T_USR      0x12
#define CBM_T_REL      0x13
#define CBM_T_VRP      0x14
#define CBM_T_DEL      0x00
#define CBM_T_CBM      0x01
#define CBM_T_DIR      0x02
#define CBM_T_LNK      0x03
#define CBM_T_OTHER    0x04
#define CBM_T_HEADER   0x05

uchar
ultimem_read_byte (upos p)
{
    return store[p];
}

void
ultimem_write_byte (upos p, uchar v)
{
    if (p > sizeof (store)) {
        printf ("ERROR: Image full.\n");
        exit (-1);
    }
    store[p] = v;
}

#else

uchar __cc65fastcall__
ultimem_read_byte (upos p)
{
    uchar * addr = (void *) ((((unsigned) p) & 0x1fff) | 0xa000u);
    unsigned        oldbank = *ULTIMEM_BLK5;
    uchar   oldcfg = *ULTIMEM_CONFIG2;
    uchar   v;

    *ULTIMEM_CONFIG2 = *ULTIMEM_CONFIG2 & 0x3f | 0x40;
    *ULTIMEM_BLK5    = p >> 13;
    v = *addr;
    //*ULTIMEM_CONFIG2 = oldcfg;
    //*ULTIMEM_BLK5    = oldbank;

    return v;
}

void __cc65fastcall__
ultimem_write_byte (upos p, uchar v)
{
    uchar *   addr = (void *) ((((unsigned) p) & 0x1fff) | 0xa000u);
    unsigned  oldbank = *ULTIMEM_BLK5;
    uchar     oldcfg = *ULTIMEM_CONFIG2;

    *ULTIMEM_CONFIG2 = *ULTIMEM_CONFIG2 & 0x3f | 0x40;
    *ULTIMEM_BLK5    = p >> 13;
    ultimem_burn_byte ((unsigned short) addr, v);
    //*ULTIMEM_CONFIG2 = oldcfg;
    //*ULTIMEM_BLK5    = oldbank;
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
        *dest++ = ultimem_read_byte (p++);
}


/*
 * BLOCK FUNCTIONS
 */

uchar __cc65fastcall__
block_get_type (upos p)
{
    return ultimem_read_byte (p + offsetof (block, type));
}

uchar __cc65fastcall__
block_get_name_length (upos p)
{
    return ultimem_read_byte (p + offsetof (block, name_length));
}

uchar __cc65fastcall__
block_get_name (upos p, uchar i)
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
    upos r = ultimem_read_int (p + offsetof (block, size));
    return r;
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
return p;

    if (p == ULTIFS_UNUSED)
        return p;

    while (1) {
        r = block_get_replacement (p);
        if (r == ULTIFS_UNUSED)
            break;
        p = r;
    }

    return p;
}

upos __cc65fastcall__
block_directory_get_first (upos parent)
{
    upos d = ultimem_read_int (file_data (parent));

    if (d == ULTIFS_UNUSED)
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
        if (n == ULTIFS_UNUSED)
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
bfile_open (upos directory, upos p, char mode)
{
    bfile * b = calloc (1, sizeof (bfile));

    p = block_get_latest_version (p);
    b->start = p;
    b->ptr = file_data (p);
    b->pos = 0;
    b->directory = directory;
    if (!mode)
        b->size = block_get_size (p);
    b->mode = mode;

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
    b->mode = ULTIFS_MODE_WRITE;

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

char __cc65fastcall__
bfile_read (bfile * b)
{
    char x;

    ultifs_error = ULTIFS_ERR_OK;
    if (b->pos >= b->size) {
        ultifs_error = ULTIFS_ERR_END_OF_FILE;
        return 0;
    }
    if (b->mode != ULTIFS_MODE_READ) {
        ultifs_error = ULTIFS_ERR_FILE_NOT_IN;
        return 0;
    }

    x = ultimem_read_byte (b->ptr);
    b->ptr++;
    b->pos++;

    return x;
}

void __cc65fastcall__
bfile_write (bfile * b, char byte)
{
    ultifs_error = ULTIFS_ERR_OK;
    if (b->mode != ULTIFS_MODE_WRITE) {
        ultifs_error = ULTIFS_ERR_FILE_NOT_OUT;
        return;
    }
    ultimem_write_byte (b->ptr, byte);
    b->ptr++;
    b->pos++;
    b->size++;
}

void __cc65fastcall__
bfile_writem (bfile * b, char * bytes, unsigned len)
{
    ultifs_error = ULTIFS_ERR_OK;
    if (b->mode != ULTIFS_MODE_WRITE) {
        ultifs_error = ULTIFS_ERR_FILE_NOT_OUT;
        return;
    }
    while (len--)
        bfile_write (b, *bytes++);
}

#ifdef __CC65__
#pragma codesize (push, 1000)
int __cc65fastcall__
bfile_readm (bfile * b, char * bytes, unsigned len)
{
    cc65register unsigned  oldbank = *ULTIMEM_BLK5;
    cc65register char      oldcfg = *ULTIMEM_CONFIG2;
    cc65register char      newcfg = oldcfg & 0x3f | 0x40;
    cc65register upos      ptr = b->ptr;
    cc65register char *    addr = (char *) ((unsigned short) ptr & 0x1fff | 0xa000);
    cc65register unsigned  bank = ptr >> 13;
    cc65register int       size = 0;
    cc65register upos      end = file_data (b->start) + b->size;
    cc65register char      v;

    ultifs_error = ULTIFS_ERR_OK;
    if (b->mode != ULTIFS_MODE_READ) {
        ultifs_error = ULTIFS_ERR_FILE_NOT_IN;
        return 0;
    }

    while (len && ptr != end) {
        --len;
        *ULTIMEM_CONFIG2 = newcfg;
        *ULTIMEM_BLK5 = bank;
        v = *addr;
        ++addr;
        *ULTIMEM_CONFIG2 = oldcfg;
        *ULTIMEM_BLK5 = oldbank;

        *bytes++ = v;
        ++ptr;

        if (addr == (char *) 0xc000u) {
            addr = (char *) 0xa000u;
            ++bank;
        }
        ++size;
    }

    b->ptr = ptr;
    return size;
}
#pragma codesize (pop)

#endif

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

    if (b->directory != p) {
        p = block_directory_get_first (b->directory);
        if (p == ULTIFS_UNUSED) {
            // Create pointer to first file of directory.
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
    if (!b->mode)
        goto read_mode;

    block_set_size (b->start, b->size);
    last_free = b->ptr;

    /* Connect file to directory tree. */
    if (b->replaced)
        bfile_link_replacement (b);
    else
        bfile_append_to_directory (b);

read_mode:
    free (b);
}

upos __cc65fastcall__
bfile_create_directory (upos parent, char * name)
{
    upos d;

    bfile * b = bfile_create (parent, name, CBM_T_DIR);

    // Make empty pointer to first file.
    bfile_write (b, 255);
    bfile_write (b, 255);
    bfile_write (b, 255);
    bfile_write (b, 255);

    d = b->start;
    bfile_close (b);

    return d;
}

#define IS_BFILE_REMOVED(p) (!block_get_size (p))

#ifdef __CC65__

upos __cc65fastcall__
bfile_lookup_name (upos p, char * name, char namelen)
{
    char * buf = malloc (namelen);

    if (!p)
        p = ULTIFS_START;

    do {
        if (namelen != block_get_name_length (p))
            continue;
        ultimem_readm (buf, namelen, p + offsetof (block, name_length) + 1);
        if (!memcmp (buf, name, namelen))
            break;
    } while (ULTIFS_UNUSED != (p = block_get_next (p)));

    free (buf);

    if (p == ULTIFS_UNUSED)
        return 0;
    return block_get_latest_version (p);
}

bfile * __cc65fastcall__
ultifs_open (upos directory, char * name, char mode)
{
    upos file = bfile_lookup_name (directory, name, strlen (name));

    if (!file)
        return NULL;
    return bfile_open (directory, file, mode);
}

bfile * __cc65fastcall__
ultifs_create (upos directory, char * name, char type)
{
    if (bfile_lookup_name (directory, name, strlen (name)))
        return NULL;
    return bfile_create (directory, name, type);
}

#endif

upos current_directory;
upos ultifs_pwd;

char
ultifs_opendir ()
{
    current_directory = ultifs_pwd;

    return 0;
}

#ifdef __CC65__

char __cc65fastcall__
ultifs_readdir (struct cbm_dirent * dirent)
{
    char type = 0;
    char name_length;
    char i;
    usize size;
    unsigned short blocks;

    if (current_directory == ULTIFS_UNUSED)
        return 1;

    dirent->type = block_get_type (current_directory);

    name_length = block_get_name_length (current_directory);
    for (i = 0; i < name_length; i++)
        dirent->name[i] = block_get_name (current_directory, i);
    dirent->name[i] = 0;

    size = block_get_size (current_directory);
    blocks = size / 254;
    if (size % 254)
        blocks++;
    if (!blocks)
        blocks = 1;
    dirent->size = blocks;

    current_directory = block_get_next (current_directory);
    return 0;
}

void
ultifs_closedir ()
{
}

upos __cc65fastcall__
ultifs_enterdir (upos parent, char * name)
{
    upos start;

    bfile * b = ultifs_open (parent, name, 0);
    if (!b)
        return -1;
    parents[current_parent++] = ultifs_pwd;
    bfile_readm (b, (void *) &ultifs_pwd, 4);
    start = b->start;
    bfile_close (b);

    return start;
}

void
ultifs_leavedir ()
{
    if (!current_parent)
        return;

    ultifs_pwd = parents[--current_parent];
}
#endif

void
ultifs_mount_traverse (upos dir)
{
    upos p;
    upos n;

    while (1) {
        dir = block_get_latest_version (dir);
        if (block_get_type (dir) == CBM_T_DIR)
            ultifs_mount_traverse (ultimem_read_int (file_data (dir)));
        n = block_get_next (dir);
        if (n != ULTIFS_UNUSED) {
            dir = n;
            continue;
        }
        p = file_data (dir) + block_get_size (dir);
        if (last_free < p)
            last_free = p;
        break;
    }
}

char
ultifs_mount ()
{
    ultifs_pwd = last_free = ULTIFS_START;
    if (ultimem_read_int (ULTIFS_START) != ULTIFS_UNUSED)
        ultifs_mount_traverse (ULTIFS_START);

    return ultimem_read_int (file_data (last_free)) != ULTIFS_UNUSED;
}

#ifndef __CC65__

#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <error.h>
#include <errno.h>

void
mkfs ()
{
    memset (store, 0xff, STORE_SIZE);
    ultifs_pwd = last_free = ULTIFS_START;
}

void
load_file (upos dir, char * name, char * pathname)
{
    FILE * f;
    usize size;
    void * data;
    bfile * b;

#ifdef COMPRESS_FILE_DATA_WITH_EXOMIZER
    if (dir) {
        if (!fork ())
            execl ("/usr/local/bin/exomizer", "exomizer", "raw", "-B", "-o", "tmp.prg", pathname, NULL);
        else
            wait (NULL);
        pathname = "tmp.prg";
    }
#endif
    
    f = fopen (pathname, "rb");
    if (!f)
        error (EXIT_FAILURE, errno, "Can't exomize file '%s'.", pathname);
    fseek (f, 0, SEEK_END);
    size = ftell (f);
    b = bfile_create (dir, name, CBM_T_PRG);
    if (!(data = malloc (size)))
        printf ("ERROR: Cannot allocate memory of size %d.\n", size);
    fseek (f, 0, 0);
    fread (data, size, 1, f);
    fclose (f);
    bfile_writem (b, data, size);
    free (data);
    bfile_close (b);
}

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
    if (!f)
        error (EXIT_FAILURE, errno, "Can't load boot file '%s'.", pathname);
    fread (store, 65536, 1, f);
    fclose (f);
    printf ("Loaded boot file '%s'.\n", pathname);
}

void
write_image (char make_truncated)
{
    FILE * img = fopen (image_name, "w");
    if (!img)
        error (EXIT_FAILURE, errno, "Can't write image '%s'.", image_name);
    fwrite (store, make_truncated ? last_free : STORE_SIZE, 1, img);
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
    mkfs ();
    i = 2;
    do {
        char * command = argv[i++];
        if (strlen (command) != 1)
            invalid ("Command must be a single character.");
        switch (*command) {
            case 'n':
                mkfs ();
                printf ("Created new image.\n");
                continue;

            case 'l':
                if (i == argc)
                    invalid ("Command 'l': Path of boot file missing.");
                load (argv[i++]);
                continue;

            case 'i':
                if (i == argc)
                    invalid ("Command 'i': Path of directory to import missing.");
                printf ("Recurisvely importing directory '%s'…\n", argv[i]);
                import_directory (ULTIFS_START, argv[i++], 0);
                continue;

            case 'w':
                write_image (0);
                printf ("Image written.\n");
                continue;

            case 'W':
                write_image (1);
                printf ("Short image (not filled up to 8MB Flash ROM size) written.\n");
                continue;

            default:
                invalid ("Unknown command.");
        }
    } while (i < argc);

    return 0;
}

#endif
