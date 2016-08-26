#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define IMAGE_SIZE  (8 * 1024 * 1024)

#define ULTIFS_ID       "ULTIFS"
#define JOURNAL_START   (64 * 1024)
#define JOURNAL_SIZE    (128 * 1024)
#define BLOCKS_START    (JOURNAL_START + JOURNAL_SIZE)
#define BLOCKS_SIZE     (128 * 1024)
#define FILES_START     (BLOCKS_START + BLOCKS_SIZE)
#define FILES_SIZE      (128 * 1024)
#define DATA_START      (FILES_START + FILES_SIZE)

char image[IMAGE_SIZE];

#pragma pack(push, 1)   /* Disable struct padding. */

/*
 * File system/journal header
 */
struct ultifs_header {
    char    is_active;
    char    id[sizeof (ULTIFS_ID)];
    int16_t version;
};

struct block {
    int16_t update;
    int32_t pos;
    int16_t size;
    int16_t next;
};

struct file {
    int16_t update;
    int16_t first_block;
};

#define MAX_FILENAME_LENGTH     16

struct ultifs_dirent {
    short   update;
    char    name[MAX_FILENAME_LENGTH];
    char    type;
    short   file;
    long    size;
};

#pragma pack(pop)

void
clear_image ()
{
    memset (image, 0xff, IMAGE_SIZE);
}

void
img_write_mem (int pos, void * str, size_t size)
{
    printf ("%d\n", size);
    memcpy (&image[pos], str, size);
}

#define IMG_WRITE_STRUCT(pos, x)    img_write_mem (pos, &x, sizeof (x))

void
img_write_string (int pos, char * str)
{
    strcpy (&image[pos], str);
}

void
img_write_char (int pos, char x)
{
    *((char *) &image[pos]) = x;
}

void
mk_bootloader ()
{
    FILE * in;

    in = fopen ("src/flashboot/flashboot.bin", "r");
    fread (image, 65536, 1, in);
    fclose (in);
}

void
mk_journal ()
{
    struct ultifs_header header;

    header.is_active = 0;
    strcpy (header.id, ULTIFS_ID);
    header.version = 1;
    IMG_WRITE_STRUCT(JOURNAL_START, header);
}

#define INITIAL_ROOT_DIRECTORY_SIZE     1024

void
mk_root_directory_block ()
{
    struct block block;

    block.update = -1;
    block.pos = DATA_START;
    block.size = INITIAL_ROOT_DIRECTORY_SIZE;
    IMG_WRITE_STRUCT(BLOCKS_START, block);
}

void
mk_root_directory_file ()
{
    struct file file;

    file.update = -1;
    file.first_block = 0;
    IMG_WRITE_STRUCT(FILES_START, file);
}

void
mkfs ()
{
    clear_image ();
    mk_bootloader ();
    mk_journal ();
    mk_root_directory_block ();
    mk_root_directory_file ();
}

void
emit_image ()
{
    FILE * out;

    out = fopen ("image", "w");
    fwrite (image, IMAGE_SIZE, 1, out);
    fclose (out);
}

int
main (int argc, char ** argv)
{
    mkfs ();

    return 0;
}
