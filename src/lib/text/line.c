#include <string.h>
#include <stdlib.h>

#include <stdio.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/line.h>
#include <lib/text/motion.h>
#include <lib/term/libterm.h>


line *    first_line;
int       linenr;
unsigned  num_lines;

char str[128];

void
line_init ()
{
    first_line = line_alloc ();
    linenr = 0;
    num_lines = 1;
}

void
line_test (char * msg)
{
    line * l = first_line;
    line * prev = NULL;
    int linenr = 0;

    while (l) {
        linenr++;
        if (l->prev != prev) {
            term_puts (msg);
            sprintf (str, "Broken link to prev on line %d.", linenr);
            term_puts (str);
            while (1);
        }

        prev = l;
        l = l->next;
    }
}

line *
line_alloc ()
{
    line * l = malloc (sizeof (line));

    l->data = NULL;
    l->prev = l->next = NULL;
    l->length = 0;

    return l;
}

void
line_insert_before ()
{
    line * current_line = line_get (linenr);
    line * new = line_alloc ();

    num_lines++;

    new->prev = current_line->prev;
    new->next = current_line;
    current_line->prev = new;

    if (current_line == first_line)
        first_line = new;
}

void
line_insert_after ()
{
    line * current_line = line_get (linenr);
    line * new = line_alloc ();

    num_lines++;

    new->prev = current_line;
    new->next = current_line->next;
    if (current_line->next)
        current_line->next->prev = new;
    current_line->next = new;

    line_test ("line insert_after");
}

void
line_delete ()
{
    line * current_line = line_get (linenr);
    line * prev = current_line->prev;
    line * next = current_line->next;

    num_lines--;

    if (prev)
        prev->next = next;
    else
        first_line = next;

    free (current_line->data);
    free (current_line);

    if (next)
        next->prev = prev;

    if (linenr >= num_lines)
        linenr = num_lines - 1;

    if (!num_lines)
        line_init ();
}

line *
line_get (unsigned i)
{
    line * l = first_line;

    while (l && i--)
        l = l->next;

    return l;
}

void
buf_to_line ()
{
    line * current_line = line_get (linenr);
    char * data;

    free (current_line->data);
    data = malloc (linebuf_length);
    current_line->length = linebuf_length;
    current_line->data = data;
    memcpy (data, linebuf, linebuf_length);
}

void
line_to_buf ()
{
    line * current_line = line_get (linenr);

    memcpy (linebuf, current_line->data, current_line->length);
    linebuf_length = current_line->length;
}

void
line_split ()
{
    line * current_line = line_get (linenr);
    line *  new;
    char *  upper_data;

    line_insert_after ();
    new = current_line->next;
    new->length = current_line->length - xpos;
    new->data = malloc (new->length);
    memcpy (new->data, &current_line->data[xpos], new->length);

    upper_data = malloc (xpos);
    memcpy (upper_data, current_line->data, xpos);
    free (current_line->data);
    current_line->data = upper_data;
    current_line->length = xpos;
}

void
line_join ()
{
    line *    current_line = line_get (linenr);
    line *    this = current_line;
    line *    next = this->next;
    char *    new_data;
    unsigned  new_len;

    if (!next)
        return;

    new_len = this->length + next->length;
    new_data = malloc (new_len);

    memcpy (new_data, this->data, this->length);
    memcpy (&new_data[this->length], next->data, next->length);

    free (this->data);
    this->data = new_data;
    this->length = new_len;

    move_down ();
    line_delete ();
    if (this->next)
        move_up ();
}

void
line_clear ()
{
    line * l = first_line;

    while (l) {
        free (l->data);
        free (l);
        l = l->next;
    }

    line_init ();
}
