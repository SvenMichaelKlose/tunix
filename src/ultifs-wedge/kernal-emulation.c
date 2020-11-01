#pragma code-name ("ULTIFS")

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ultifs.h"
#include "../lib/ultimem/ultimem.h"

#define _FNLEN()    (*(char*) 0xb7)     // File name length
//#define _LFN()      (*(char*) 0xb8)     // Logical file
#define _SA()       (*(char*) 0xb9)     // Secondary address
#define _FA()       (*(char*) 0xba)     // Device number
#define _FNAME()    ((char*) 0xbb)      // File name

#define STATUS      ((char*) 0x90)      // Serial status byte
#define STATUS_NO_DEVICE        0x80
#define STATUS_END_OF_FILE      0x40
#define STATUS_CHECKSUM_ERROR   0x20
#define STATUS_READ_ERROR       0x10
#define STATUS_LONG             0x08
#define STATUS_SHORT            0x04
#define STATUS_TIMEOUT_READ     0x02
#define STATUS_TIMEOUT_WRITE    0x01

#define ERR_BYTE_DECODING       24
#define ERR_WRITE               25
#define ERR_WRITE_PROTECT_ON    26
#define ERR_SYNTAX              30
#define ERR_INVALID_COMMAND     31
#define ERR_LONG_LINE           32
#define ERR_INVALID_FILE_NAME   33
#define ERR_NO_FILE_GIVEN       34
#define ERR_FILE_TOO_LARGE      52
#define ERR_FILE_OPEN_FOR_WRITE 60
#define ERR_FILE_NOT_OPEN       61
#define ERR_FILE_NOT_FOUND      62
#define ERR_FILE_EXISTS         63
#define ERR_FILE_TYPE_MISMATCH  64
#define ERR_NO_CHANNEL          70
#define ERR_DISK_FULL           72

#define accu        (*(char*) 0x100)
#define xreg        (*(char*) 0x101)
#define yreg        (*(char*) 0x102)
#define flags       (*(char*) 0x103)

extern void ultifs_kopen (void);
extern void ultifs_kclose (void);

typedef struct _channel {
    char *      name;
    bfile *     file;

    // Directory/error status.
    char *      out;
    char *      outptr;
} channel;

channel * channels[256];

void
init_kernal_emulation ()
{
    bzero (channels, sizeof (channels));
}

void
copy_from_process (char * from, char * to, char len)
{
    char blk5 = *ULTIMEM_BLK5;
    char * ptr;

    while (len--) {
        ptr = ultimem_map_ptr (from++, (void *) 0xa000, (int *) 0x104, ULTIMEM_BLK5);
        *to = *ptr;
    }

    *ULTIMEM_BLK5 = blk5;
}

char * last_error = NULL;

void
set_error (char code)
{
    if (last_error)
        free (last_error);
    last_error = malloc (64);
    sprintf (last_error, "%d, ERROR, 0, 0", code);
}

void
open_command ()
{
}

void
make_directory_list ()
{
}

void
ultifs_kopen ()
{
    char * name;
    bfile * found_file;
    channel * lf;

    if (channels[_SA()]) {
        set_error (ERR_NO_CHANNEL);
        return;
    }

    name = malloc (_FNLEN() + 1);
    if (!name) {
        set_error (ERR_BYTE_DECODING);
    }
    copy_from_process (_FNAME(), name, _FNLEN());
    name[_FNLEN()] = 0;

    if (_SA() == 15) {
        open_command ();
        return;
    }

    if (_FNLEN() == 1 && *name == '$') {
        make_directory_list ();
        return;
    }

    found_file = ultifs_open (ultifs_pwd, name, 0);
    if (!found_file) {
        set_error (ERR_FILE_NOT_FOUND);
        return;
    }

    lf = malloc (sizeof (channel));
    lf->file = found_file;
    channels[_SA()]->file = found_file;

    *STATUS = 0;
}

void
ultifs_kclose ()
{
    channel * file = channels[_SA()];

    if (!file) {
        set_error (ERR_FILE_NOT_OPEN);
        return;
    }

    if (_SA() == 15) {
        ultifs_kclall ();
        return;
    }

    bfile_close (file->file);
    free (file);
    channels[_SA()] = NULL;
}

void
ultifs_kchkin ()
{
}

void
ultifs_kchkout ()
{
}

void
ultifs_kclrcn ()
{
}

void
ultifs_kbasin ()
{
}

void
ultifs_kbasout ()
{
    channel * file = channels[_SA()];

    // TODO check if there's enough space left.
    if (!file) {
        *STATUS = STATUS_READ_ERROR;
        return;
    }

    //bfile_write (file, x);
}

void
ultifs_kclall ()
{
}
