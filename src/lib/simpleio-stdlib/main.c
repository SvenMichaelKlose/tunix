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

FILE *      channels[MAX_CHANNELS];
signed char last_error;

bool
raw_eof (void)
{
    return feof (channels[(int) fnin]);
}

signed char
raw_err (void)
{
    return last_error;
}

char
raw_in (void)
{
    int c;

    last_error = 0;
    c = fgetc (channels[(int) fnin]);
    if (c == EOF) {
        c = 0;
        if (errno)
            last_error = errno;
    }
    return c;
}

void
raw_out (char c)
{
    last_error = 0;
    if (EOF == fputc (c, channels[(int) fnout]))
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
    (void) c;
}

simpleio_chn_t
alloc_channel (FILE * handle)
{
    int i;

    for (i = MIN_CHANNEL; i < MAX_CHANNELS; i++)
        if (!channels[i]) {
            channels[i] = handle;
            return i;
        }

    return 0;
}

void
raw_close (simpleio_chn_t c)
{
    fclose (channels[(int) c]);
    channels[(int) c] = NULL;
}

simpleio_chn_t 
simpleio_open (char * name, char mode)
{
    FILE * handle;
    char m[2];

    last_error = 0;
    m[0] = mode;
    m[1] = 0;
    if ((handle = fopen (name, m)))
        return alloc_channel (handle);
    return 0;
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
    channels[STDIN]  = stdin;
    channels[STDOUT] = stdout;
    channels[STDERR] = stderr;
}
