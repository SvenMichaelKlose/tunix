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
    line * ls = malloc (sizeof (line));
    ls->prev = ls->next = NULL;

    return ls;
}

void
linelist_insert_before ()
{
    line * new = line_alloc ();

    num_lines++;

    new->prev = current_line->prev;
    new->next = current_line;
    current_line->prev = new;
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
    line  * next;

    num_lines--;

    next = current_line->next;

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
    line  * l = first_line;

    while (l && i--)
        l = l->next;

    return l;
}

void
copy_linebuf_to_current_line ()
{
    char * data;

    free (current_line->data);
    data = malloc (linebuf_length);
    current_line->data = data;
    memcpy (linebuf, data, linebuf_length);
}

void
copy_currnet_line_to_linebuf ()
{
    linebuf_length = strlen (current_line->data);
    memcpy (linebuf, current_line->data, linebuf_length);
}

void
linelist_goto (unsigned n)
{
    copy_linebuf_to_current_line ();
    current_line = linelist_get (n);
    copy_currnet_line_to_linebuf ();
}

void
linelist_init ()
{
    first_line = current_line = line_alloc ();
    linenr = 0;
    num_lines = 1;
}
