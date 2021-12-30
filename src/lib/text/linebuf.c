#include <string.h>

#include <libterm.h>

#include "linebuf.h"


char        linebuf[MAX_LINE_LENGTH + 1];
unsigned    linebuf_length;


void
linebuf_clear ()
{
    linebuf[0] = 0;
    linebuf_length = 0;
}

void
linebuf_insert_char (unsigned p, char c)
{
    if (linebuf_length >= MAX_LINE_LENGTH)
        return;

    memmove (&linebuf[p + 1], &linebuf[p], linebuf_length - p);
    linebuf[p] = c;
    linebuf_length++;
}

void
linebuf_delete_char (unsigned p)
{
    if (!linebuf_length || !p)
        return;

    memmove (&linebuf[p - 1], &linebuf[p], linebuf_length - p);
    linebuf_length--;
}
