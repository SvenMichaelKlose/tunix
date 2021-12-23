#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"


line      * first_line;
line      * current_line;
int       linenr;
unsigned  num_lines;


line *
line_alloc ()
{
    line * l = malloc (sizeof (line));

    l->prev = l->next = l->data = NULL;
    l->length = 0;

    return l;
}

void
linelist_insert_before ()
{
    line * new = line_alloc ();

    num_lines++;

    new->prev = current_line->prev;
    new->next = current_line;
    current_line->prev = new;
    if (current_line == first_line)
        first_line = new;
}

void
linelist_insert_after ()
{
    line * new = line_alloc ();

    num_lines++;

    new->prev = current_line;
    new->next = current_line->next;
    current_line->next = new;
}

void
linelist_delete ()
{
    line  * next = current_line->next;

    num_lines--;

    if (first_line == current_line)
        first_line = next;
    else
        current_line->prev->next = next;

    if (next)
        next->prev = current_line->prev;

    free (current_line);
    current_line = next;
}

line *
linelist_get (unsigned i)
{
    line * l = first_line;

    while (l && i--)
        l = l->next;

    return l;
}

void
linelist_buf_to_line ()
{
    char * data;

    free (current_line->data);

    data = malloc (linebuf_length);

    current_line->length = linebuf_length;
    current_line->data = data;

    memcpy (data, linebuf, linebuf_length);
}

linelist_goto (unsigned n)
{
    current_line = linelist_get (n);
}

void
linelist_line_to_buf ()
{
    linelist_goto (linenr);
    memcpy (linebuf, current_line->data, linebuf_length);
}

void
linelist_split ()
{
    line *   new;

    if (xpos == current_line->length)
        return;

    linelist_insert_after ();
    new = current_line->next;
    new->length = current_line->length - xpos;
    new->data = malloc (new->length);
    memcpy (new->data, &current_line->data[xpos], new->length);
    current_line->length = xpos;
}

void
linelist_init ()
{
    first_line = current_line = line_alloc ();
    linenr = 0;
    num_lines = 1;
}
