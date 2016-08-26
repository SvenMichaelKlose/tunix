#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define IMAGE_SIZE  (8 * 1024 * 1024)

#define ULTIFS_ID       "ULTIFS"
#define JOURNAL_START   (64 * 1024)

char image[IMAGE_SIZE];

/*
 * File system/journal header
 */
struct ultifs_header {
    char    id[sizeof (ULTIFS_ID)];
    short   version;
};

void
clear_image ()
{
    memset (image, 0xff, IMAGE_SIZE);
}

void
img_write_mem (int pos, void * str, size_t size)
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

    strcpy (header.id, ULTIFS_ID);
    header.version = 1;
    IMG_WRITE_STRUCT(JOURNAL_START, header);
}

void
mkfs ()
{
    FILE * out;

    clear_image ();
    mk_bootloader ();
    mk_journal ();

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
