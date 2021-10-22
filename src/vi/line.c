#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "line.h"

typedef unsigned pos_t;

#define FALSE   0
#define TRUE    1

line        * first_line = NULL;
line        * current_line = NULL;
unsigned    linenr = 0;
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


///////////////
// LINE LIST //
///////////////

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
line_insert ()
{
    line  * new = line_alloc ();
    line  * prev;

    num_lines++;

    if (!first_line) {
        first_line = current_line = new;
        return;
    }
    if (!current_line) {
        current_line = first_line;
        prev = NULL;
    } else
        prev = current_line->prev;

    if (current_line == first_line)
        first_line = new;
    else
        prev->next = new;

    new->prev = prev;
    new->next = current_line;
    current_line = new;
}

void
line_last ()
{
    current_line = first_line;

    while (current_line && current_line->next)
        current_line = current_line->next;
}

void
line_append ()
{
    line  * new = line_alloc ();

    num_lines++;

    if (!current_line)
        line_last ();
    else if (current_line->next) {
        term_puts ("!!! ");
        term_puts (&current_line->next->data);
        our_error (": Cannot append before.");
    }

    current_line->next = new;
    new->prev = current_line;
    current_line = new;

    line_move_down ();
}

void
line_delete ()
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
line_get (unsigned i)
{
    line  * l = first_line;

    while (l && i--)
        l = l->next;

    return l;
}

void
set_current_line (unsigned n)
{
    current_line = line_get (n);
    if (!current_line) {
        term_puts ("No line at #.");
        while (1);
    }

    linebuf_length = strlen (&current_line->data);
    strcpy (linebuf, &current_line->data);
}


////////////
// MOTION //
////////////

char
line_move_down ()
{
    if (linenr == num_lines - 1)
        return FALSE;

    set_current_line (++linenr);

    if (ypos != 23)
        ypos++;

    term_put (TERM_LINE_FEED);

    return TRUE;
}


///////////////////
// SCREEN REDRAW //
///////////////////

void
screen_redraw ()
{
    line  * ls;
    char  y;

    disable_cursor ();

    term_put (TERM_CLEAR_SCREEN);
    for (y = 0; y < 24; y++) {
        if (y == linenr)
            print_linebuf ();
        else {
            if (ls = line_get ((unsigned) y))
                term_puts (&ls->data);
            else
                term_put (0x7e);
        }

        if (y < 23) {
            term_put (TERM_CARRIAGE_RETURN);
            term_put (TERM_LINE_FEED);
        }
    }

    set_cursor ();
    enable_cursor ();
}


//////////////
// COMMANDS //
//////////////

void
line_open_below ()
{
    line_clear ();

    if (line_move_down ())
        line_insert ();
    else {
        line_append ();
        line_move_down ();
    }

    term_put (TERM_INSERT_LINE);
}

void
line_init ()
{
    line_clear ();
    line_insert ();
}
