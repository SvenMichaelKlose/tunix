#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define FALSE   0
#define TRUE    1

#define IMAGE_SIZE  (8 * 1024 * 1024)

#define ULTIFS_ID       "ULTIFS"
#define JOURNAL_START   (64 * 1024)
#define JOURNAL_SIZE    (64 * 1024)
#define BLOCKS_START    (JOURNAL_START + JOURNAL_SIZE)
#define BLOCKS_SIZE     (64 * 1024)
#define FILES_START     (BLOCKS_START + BLOCKS_SIZE)
#define FILES_SIZE      (64 * 1024)
#define DATA_START      (FILES_START + FILES_SIZE)

char image[IMAGE_SIZE];

int32_t journal;
int32_t journal_free;
int32_t blocks;
int32_t blocks_free;
int32_t files;
int32_t files_free;

#ifdef __GNUC__
#pragma pack(push, 1)   /* Disable struct padding. */
#endif

/*
 * File system/journal header
 */
struct ultifs_header {
    char    is_active;
    char    id[sizeof (ULTIFS_ID)];
    int16_t version;
    int32_t blocks;
    int32_t files;
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

#ifdef __GNUC__
#pragma pack(pop)
#endif

void
clear_image ()
{
    memset (image, 0xff, IMAGE_SIZE);
}

void
img_write_mem (int32_t pos, void * str, size_t size)
{
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
img_read_mem (void * dest, int32_t pos, size_t size)
{
    memcpy (dest, &image[pos], size);
}

#define IMG_READ_STRUCT(x, pos)    img_read_mem (&x, pos, sizeof (x))

char
img_read_char (int32_t pos)
{
    return ((char *) &image)[pos];
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

int32_t
find_end (int32_t start, size_t record_size, int num_records)
{
    int32_t p;
    char i;
    int j;
    char is_free;
    int n = 0;

    p = start;
    for (j = 0; j < num_records; j++) {
        is_free = TRUE;
        for (i = 0; i < record_size; i++) {
            if (img_read_char (p + i) == -1)
                continue;
            is_free = FALSE;
            break;
        }
        if (is_free)
            break;
        p += record_size;
        n++;
    }
    return p;
}

void
mount_journal (struct ultifs_header * h)
{
    journal = JOURNAL_START;
    journal_free = find_end (JOURNAL_START + sizeof (struct ultifs_header), 8, (JOURNAL_SIZE - sizeof (struct ultifs_header)) / 8);
}

void
mount_blocks (struct ultifs_header * h)
{
    blocks = h->blocks;
    blocks_free = find_end (BLOCKS_START, sizeof (struct block), BLOCKS_SIZE / sizeof (struct block));
}

void
mount_files (struct ultifs_header * h)
{
    files = h->files;
    files_free = find_end (FILES_START, sizeof (struct file), FILES_SIZE / sizeof (struct file));
}

void
mount ()
{
    struct ultifs_header h;

    IMG_READ_STRUCT(h, JOURNAL_START);
    mount_journal (&h);
    mount_blocks (&h);
    mount_files (&h);
}

int
main (int argc, char ** argv)
{
    mkfs ();
    mount ();

    return 0;
}
