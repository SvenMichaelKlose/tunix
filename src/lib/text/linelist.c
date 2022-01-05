#include <string.h>
#include <stdlib.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>


line      * first_line;
line      * current_line;
int       linenr;
unsigned  num_lines;


line *
line_alloc ()
{
    line * l = malloc (sizeof (line));

    l->prev = l->next = NULL;
    l->data = NULL;
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

    if (current_line == first_line)
        current_line = next;
    else
        current_line->prev->next = next;

    if (next)
        next->prev = current_line->prev;

    free (current_line->data);
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

void
linelist_goto (unsigned n)
{
    current_line = linelist_get (n);
}

void
linelist_line_to_buf ()
{
    memcpy (linebuf, current_line->data, current_line->length);
    linebuf_length = current_line->length;
}

void
linelist_split ()
{
    line *  new;
    char *  upper_data;

    if (xpos == current_line->length)
        return;

    linelist_insert_after ();
    new = current_line->next;
    new->length = current_line->length - xpos;
    new->data = malloc (new->length);
    memcpy (new->data, &current_line->data[xpos], new->length);

    if (xpos) {
        upper_data = malloc (xpos);
        memcpy (upper_data, current_line->data, xpos);
        free (current_line->data);
        current_line->data = upper_data;
        current_line->length = xpos;
    } else {
        free (current_line->data);
        current_line->data = NULL;
        current_line->length = 0;
    }
}

void
linelist_join ()
{
    line *    next = current_line->next;
    unsigned  len = current_line->length;
    char *    new_data;

    if (!next)
        return;

    len += next->length;
    new_data = malloc (len); // TODO: Barf on low memory.

    memcpy (new_data, current_line->data, current_line->length);
    memcpy (&new_data[current_line->length], next->data, next->length);

    free (current_line->data);
    current_line->data = new_data;
    current_line->length = len;
}

void
linelist_init ()
{
    first_line = current_line = line_alloc ();
    linenr = 0;
    num_lines = 1;
}
