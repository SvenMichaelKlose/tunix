#include <string.h>

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
linebuf_insert_char (pos_t p, char c)
{
    if (linebuf_length >= MAX_LINE_LENGTH)
        return;

    memmove (&linebuf[p + 1], &linebuf[p], linebuf_length - p);
    linebuf[p] = c;
    linebuf_length++;
}

void
linebuf_delete_char (pos_t p)
{
    if (!linebuf_length)
        return;

    memmove (&linebuf[p], &linebuf[p + 1], linebuf_length - p - 1);
    linebuf_length--;
}

void
linebuf_replace_char (pos_t p, char c)
{
    if (linebuf_length)
        linebuf[p] = c;
    else
        linebuf_insert_char (p, c);
}
