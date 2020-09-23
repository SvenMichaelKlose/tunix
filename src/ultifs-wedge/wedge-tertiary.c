/*
 * KERNAL function emulation
 *
 * This is where the complex things happen, that's why
 * it is written in C.
 */

#pragma code-name ("ULTIFS")

#include <stddef.h>
#include <stdlib.h>

#include "ultifs.h"

#define _FNLEN()    (*(char*) 0xb7)     // File name length
#define _LFN()      (*(char*) 0xb8)     // Logical file
#define _SA()       (*(char*) 0xb9)     // Secondary address
#define _FA()       (*(char*) 0xba)     // Device number
#define _FNAME()    ((char*) 0xbb)      // File name

#define ERR_OUT_OF_MEMORY   1   // TODO: Figure out right code.
#define ERR_WRONG_LFN       2   // TODO: Figure out right code.
#define ERR_NOT_FOUND       3   // TODO: Figure out right code.

extern void ultifs_kopen (void);

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
        return;
    }

    if (logical_files[_LFN()]) {
        set_error (ERR_WRONG_LFN);
        return;
    }
    logical_files[_LFN()] = found_file;
    set_ok ();
}
