#pragma code-name ("ULTIFS")

#include <stddef.h>
#include <stdlib.h>

#include "ultifs.h"

#define _FNLEN()    (*(char*) 0xb7)     // File name length
#define _LFN()      (*(char*) 0xb8)     // Logical file
#define _SA()       (*(char*) 0xb9)     // Secondary address
#define _FA()       (*(char*) 0xba)     // Device number
#define _FNAME()    ((char*) 0xbb)      // File name

#define STATUS()    ((char*) 0x90)      // Serial status byte
#define STATUS_NO_DEVICE        0x80
#define STATUS_END_OF_FILE      0x40
#define STATUS_CHECKSUM_ERROR   0x20
#define STATUS_READ_ERROR       0x10
#define STATUS_LONG             0x08
#define STATUS_SHORT            0x04
#define STATUS_TIMEOUT_READ     0x02
#define STATUS_TIMEOUT_WRITE    0x01

#define ERR_OUT_OF_MEMORY   1   // TODO: Figure out right code.
#define ERR_WRONG_LFN       2   // TODO: Figure out right code.
#define ERR_NOT_FOUND       3   // TODO: Figure out right code.
#define ERR_NOT_OPEN        3   // TODO: Figure out right code.

#define accu        (*(char*) 0x100)
#define xreg        (*(char*) 0x101)
#define yreg        (*(char*) 0x102)
#define flags       (*(char*) 0x103)

extern void ultifs_kopen (void);
extern void ultifs_kclose (void);

bfile * logical_files[256];

void
ultifs_kinit ()
{
    char i;

    do {
        logical_files[i] = NULL;
    } while (++i);
}

void
set_error (char x)
{
}

void
set_ok ()
{
}

void
copy_from_process (char * from, char * to, char len)
{
    char blk5 = ultimem_blk5 ();

    do {
        ptr = ultimem_map5 (from++, 0xa000, 0xe); // Map 'from' into BLK5.
        *to = *ptr;
    } while (len--);

    ultimem_set_blk5 (blk5);
}

void
ultifs_kopen ()
{
    char * name = malloc (_FNLEN() + 1);
    bfile * found_file;

    if (!name) {
        set_error (ERR_OUT_OF_MEMORY);
        return;
    }

    copy_from_process (_FNAME(), name, _FNLEN());
    name[_FNLEN()] = 0;

    found_file = ultifs_open (ultifs_pwd, name, 0);
    if (!found_file) {
        set_error (ERR_NOT_FOUND);
        goto error;
    }

    if (logical_files[_LFN()]) {
        set_error (ERR_WRONG_LFN);
        goto error;
    }
    logical_files[_LFN()] = found_file;
    set_ok ();

    return;

error:
    free (name);
}

void
ultifs_kclose ()
{
    bfile * file = logical_files[_LFN()];

    if (!file) {
        set_error (ERR_NOT_OPEN);
        return;
    }

    bfile_close (file);
    logical_files[_LFN()] = NULL;
}

void
ultifs_kchkin ()
{
    bfile * file = logical_files[_LFN()];

    if (!file) {
        set_error (ERR_NOT_OPEN);
        return;
    }

    file->mode = ULTIFS_MODE_READ;
}

void
ultifs_kchkout ()
{
    bfile * file = logical_files[_LFN()];

    if (!file) {
        set_error (ERR_NOT_OPEN);
        return;
    }

    file->mode = ULTIFS_MODE_READ;
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
    bfile * file = logical_files[_LFN()];

    // TODO check if there's enough space left.
    if (!file) {
        set_error (ERR_NOT_OPEN);
        return;
    }

    //bfile_write (file, x);
}

void
ultifs_kclall ()
{
}
