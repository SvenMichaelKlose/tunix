#include <string.h>

#include <libterm.h>

#include "linebuf.h"


char        linebuf[MAX_LINEBUF_LENGTH];
unsigned    linebuf_length;


void
linebuf_clear ()
{
    linebuf[0] = 0;
    linebuf_length = 0;
}

void
linebuf_insert_char (pos_t p, char c)
{
    if (linebuf_length >= MAX_LINEBUF_LENGTH)
        return;

    memmove (&linebuf[p + 1], &linebuf[p], linebuf_length - p);
    linebuf[p] = c;
    linebuf_length++;
}

void
linebuf_delete_char (pos_t p)
{
    if (!linebuf_length || !p)
        return;

    memmove (&linebuf[p - 1], &linebuf[p], linebuf_length - p);
    linebuf_length--;
}
