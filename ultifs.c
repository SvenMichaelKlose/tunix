/*
 * UltiFS
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define FALSE   0
#define TRUE    1

#define IMAGE_SIZE      (8 * 1024 * 1024)

#define ULTIFS_ID       "ULTIFS"
#define JOURNAL_START   (64 * 1024)
#define JOURNAL_SIZE    (64 * 1024)
#define BLOCKS_START    (JOURNAL_START + JOURNAL_SIZE)
#define BLOCKS_SIZE     (64 * 1024)
#define NUM_BLOCKS      (BLOCKS_SIZE / sizeof (struct block))
#define FILES_START     (BLOCKS_START + BLOCKS_SIZE)
#define FILES_SIZE      (64 * 1024)
#define DATA_START      (FILES_START + FILES_SIZE)

char image[IMAGE_SIZE];

int32_t journal;
int32_t journal_free;
int32_t blocks;
int32_t files;
int32_t data_free;

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

int16_t
img_read_word (int32_t pos)
{
    return ((int16_t *) &image)[pos];
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
    header.blocks = BLOCKS_START;
    header.files = FILES_START;
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
    block.next = -1;
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

/*
 * Find a free record in journal, block chains or file index.
 */
int32_t
find_free (int32_t start, size_t record_size, int num_records)
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

/*
 * Find update of a record.
 */
int32_t
get_update (int32_t pos)
{
    int16_t update;

    while (1) {
        update = img_read_word (pos);
        if (update == -1)
            return pos;
        pos = ((int32_t) update << 3) + JOURNAL_START;
    }

    return pos;
}

void
get_block (struct block * b, int16_t idx)
{
    int32_t pos = idx * sizeof (struct block) + BLOCKS_START;
    img_read_mem (b, get_update (pos), sizeof (struct block));
}

void
put_block (struct block * b, int16_t idx)
{
    int32_t pos = idx * sizeof (struct block) + BLOCKS_START;
    img_write_mem (get_update (pos), b, sizeof (struct block));
}

void
get_file (struct file * b, int16_t idx)
{
    int32_t pos = idx * sizeof (struct file) + FILES_START;
    img_read_mem (b, get_update (pos), sizeof (struct file));
}

/*
 * Find the first officially free byte in the data area.
 */
int32_t
find_last_block_address ()
{
    struct block b;
    int i;
    int32_t highest = 0;
    int32_t tmp;

    for (i = 0; i < NUM_BLOCKS; i++) {
        get_block (&b, i);
        if (b.pos == -1)
            continue;
        tmp = b.pos + b.size;
        if (highest < tmp)
            highest = tmp;
    }
    return highest;
}

/*
 * Find the first free byte in the data area.
 *
 * Skips garbage resulting from incomplete writes.
 */
void
find_data_free ()
{
    int32_t highest = find_last_block_address ();
    int i;

    for (i = IMAGE_SIZE - 1; i > highest; i--) {
        if (img_read_char (i) == -1)
            continue;
        data_free = i + 1;
        return;
    }
    data_free = highest;
}

void
mount_journal (struct ultifs_header * h)
{
    journal = JOURNAL_START;
    journal_free = find_free (JOURNAL_START + sizeof (struct ultifs_header), 8, (JOURNAL_SIZE - sizeof (struct ultifs_header)) / 8);
}

void
mount_blocks (struct ultifs_header * h)
{
    blocks = h->blocks;
}

void
mount_files (struct ultifs_header * h)
{
    files = h->files;
}

void
mount ()
{
    struct ultifs_header h;

    IMG_READ_STRUCT(h, JOURNAL_START);
    mount_journal (&h);
    mount_blocks (&h);
    mount_files (&h);
    find_data_free ();
}

int16_t
alloc_journal (void * src, size_t size)
{
    int16_t j = journal_free >> 3;

    if (journal_free & 7)
        j++;
    img_write_mem (journal_free, src, size);
    journal_free += size;
    return j;
}

int16_t
alloc_block (int16_t size)
{
    int32_t i = find_free (BLOCKS_START, sizeof (struct block), BLOCKS_SIZE / sizeof (struct block));
    struct block b;

    b.update = -1;
    b.pos = data_free;
    b.size = size;
    b.next = -1;
    data_free += size ? size : 0x10000;
    IMG_WRITE_STRUCT(i, b);
    return (i - BLOCKS_START) >> 3;
}

int16_t
alloc_file (int16_t first_block)
{
    int32_t pos = find_free (FILES_START, sizeof (struct file), FILES_SIZE / sizeof (struct file));
    struct file f;

    f.update = -1;
    f.first_block = first_block;
    IMG_WRITE_STRUCT(pos, f);
    return (pos - FILES_START) / sizeof (struct file);
}

void
link_block (int16_t idx, int16_t next)
{
    struct block b;

    get_block (&b, idx);
    b.next = next;
    put_block (&b, idx);
}

int16_t
alloc_block_chain (size_t size)
{
    int16_t first_block = -1;
    int16_t last_block = -1;
    int16_t b;
    size_t s;

    while (size) {
        s = size > 65536 ? 65536 : size;
        b = alloc_block (s);
        if (last_block != -1)
            link_block (last_block, b);
        else
            first_block = b;
        size -= s;
        last_block = b;
    }
    return first_block;
}

int16_t
create_file (size_t size)
{
    int16_t blocks = alloc_block_chain (size);
    return alloc_file (blocks);
}

typedef void (*fileop) (int32_t pos, void * data, size_t size);

void
data_xfer (fileop op, int16_t fi, int32_t ofs, void * data, size_t size)
{
    struct file f;
    struct block b;
    int16_t bi;
    int32_t bend;
    size_t s;
    size_t bs;

    get_file (&f, fi);
    bi = f.first_block;
    while (2) {
        get_block (&b, bi);
        bs = b.size ? b.size : 0x10000;
        if (ofs < bs) {
            s = bs - ofs;
            if (s > size)
                s = size;
            op (b.pos + ofs, data, s);
            if (!(size -= s))
                return;
            data += s;
            ofs += s;
        }
        bi = b.next;
        if (bi == -1) {
            printf ("Write across block chain end.\n");
            exit (1);
        }
    }
}

void
op_write (int32_t pos, void * data, size_t size)
{
    img_write_mem (pos, data, size);
}

void
op_read (int32_t pos, void * data, size_t size)
{
    img_read_mem (data, pos, size);
}

void
write_file (int16_t fi, int32_t ofs, void * data, size_t size)
{
    data_xfer (op_write, fi, ofs, data, size);
}

void
read_file (int16_t fi, int32_t ofs, void * data, size_t size)
{
    data_xfer (op_read, fi, ofs, data, size);
}

int
main (int argc, char ** argv)
{
    int16_t f;

    mkfs ();
    mount ();
    f = create_file (0x40000);
    write_file (f, 0, "Hello world!", 12);
    emit_image ();

    return 0;
}
