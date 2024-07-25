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

FILE * handles[256];

char last_error;

bool
raw_eof (void)
{
    return feof (handles[(int) fnin]);
}

char
raw_err (void)
{
    return last_error;
}

char
raw_in (void)
{
    int c = fgetc (handles[(int) fnin]);
    if (c == EOF)
        c = 0;
    last_in = c;
    return last_in;
}

void
raw_out (char c)
{
    fputc (c, handles[(int) fnout]);
}

void
raw_setin (simpleio_chn_t c)
{
    (void) c;
}

void
raw_setout (simpleio_chn_t c)
{
    fflush (handles[(int) fnout]);
    (void) c;
}

void
raw_close (simpleio_chn_t c)
{
    fclose (handles[(int) c]);
}

void
simpleio_open (simpleio_chn_t c, char * name, char mode)
{
    char m[2];
    m[0] = mode;
    m[1] = 0;
    handles[(int) c] = fopen (name, m);
    if (!handles[(int) c]) {
        fprintf (stderr, "'%s': ", name);
        perror ("simpleio-stdlib::open()");
        last_error = c;
    }
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
    handles[STDIN] = stdin;
    handles[STDOUT] = stdout;
    handles[STDERR] = stderr;
}
