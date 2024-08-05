#ifdef __CC65__

#ifndef __CBM__
#define __CBM__
#endif

#include <cbm.h>
#include <ingle/cc65-charmap.h>

#endif // #ifdef __CC65__

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <simpleio/libsimpleio.h>

#define MIN_CHANNEL     (STDERR + 1)
#define MAX_CHANNELS    256

FILE * allocated_channels[MAX_CHANNELS];
char last_error;

bool
raw_eof (void)
{
    return feof (allocated_channels[(int) fnin]);
}

char
raw_err (void)
{
    return last_error;
}

char
raw_in (void)
{
    int c;

    last_error = 0;
    c = fgetc (allocated_channels[(int) fnin]);
    if (c == EOF) {
        c = 0;
        if (errno)
            last_error = -1;
    }
    return c;
}

void
raw_out (char c)
{
    last_error = 0;
    if (EOF == fputc (c, allocated_channels[(int) fnout]))
        last_error = -1;
}

void
raw_setin (simpleio_chn_t c)
{
    (void) c;
}

void
raw_setout (simpleio_chn_t c)
{
    fflush (allocated_channels[(int) fnout]);
    (void) c;
}

simpleio_chn_t
alloc_channel (FILE * handle)
{
    int i;

    for (i = MIN_CHANNEL; i < MAX_CHANNELS; i++)
        if (!allocated_channels[i]) {
            allocated_channels[i] = handle;
            return i;
        }

    return -1;
}

void
raw_close (simpleio_chn_t c)
{
    fclose (allocated_channels[(int) c]);
    allocated_channels[(int) c] = NULL;
}

simpleio_chn_t 
simpleio_open (char * name, char mode)
{
    FILE * handle;
    char m[2];

    m[0] = mode;
    m[1] = 0;

    handle = fopen (name, m);
    if (!handle)
        return 0;
    return alloc_channel (handle);
}

simpleio vectors = {
    raw_eof,
    raw_err,
    raw_in,
    raw_out,
    raw_setin,
    raw_setout,
    raw_close
};

void
simpleio_init ()
{
    simpleio_set (&vectors);
    allocated_channels[STDIN] = stdin;
    allocated_channels[STDOUT] = stdout;
    allocated_channels[STDERR] = stderr;
}
