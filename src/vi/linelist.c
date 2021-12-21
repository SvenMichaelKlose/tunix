#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"


line        * first_line = NULL;
line        * current_line = NULL;
int         linenr = 0;
unsigned    num_lines = 0;


void
our_error (char * txt)
{
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (23);
    term_puts (txt);
    while (1);
}


line *
line_alloc ()
{
    line  * ls = malloc (sizeof (line) + strlen (linebuf));

    if (!ls)
        our_error ("Out of memory.");

    ls->prev = ls->next = NULL;

    linebuf[linebuf_length] = 0;
    strcpy (&ls->data, linebuf);

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

    if (!current_line)
        our_error ("Can't delete nothing.");

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
linelist_goto (unsigned n)
{
    current_line = linelist_get (n);
    if (!current_line) {
        term_puts ("No line at #.");
        while (1);
    }

    linebuf_length = strlen (&current_line->data);
    strcpy (linebuf, &current_line->data);
}
