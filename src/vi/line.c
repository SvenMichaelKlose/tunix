#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>

#include "linebuf.h"
#include "line.h"

#define FALSE   0
#define TRUE    1

pos_t       xpos;
pos_t       ypos = 0;
line   * first_line = NULL;
line   * current_line = NULL;
unsigned    linenr = 0;
unsigned    num_lines = 0;


void
line_clear ()
{
    linebuf_clear ();
    xpos = 0;
}

void
set_cursor (void)
{
    term_put (TERM_SET_CURSOR);
    term_put (xpos);
    term_put (ypos);
}

void
disable_cursor ()
{
    term_put (TERM_ESCAPE);
    term_put (TERM_DISABLE_ATTR);
    term_put (TERM_ATTR_CURSOR);
}

void
enable_cursor ()
{
    term_put (TERM_ESCAPE);
    term_put (TERM_ENABLE_ATTR);
    term_put (TERM_ATTR_CURSOR);
}

line *
line_alloc ()
{
    line * ls = malloc (sizeof (line) + strlen (linebuf));

    if (!ls) {
        term_puts ("Out of memory.");
        while (1);
    }

    ls->prev = ls->next = NULL;

    linebuf[linebuf_length] = 0;
    strcpy (&ls->data, linebuf);

    return ls;
}

void
line_insert ()
{
    line * new = line_alloc ();
    line * prev;

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
line_append ()
{
    line * new = line_alloc ();

    num_lines++;

    if (!current_line) {
        current_line = first_line;

        while (current_line && current_line->next)
            current_line = current_line->next;
    } else if (current_line->next) {
        term_puts ("Cannot append before next.");
        term_puts (&current_line->next->data);
        while (1);
    }

    current_line->next = new;
    new->prev = current_line;
    current_line = new;
}

void
line_delete ()
{
    line * next;

    if (!current_line)
        return;

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

void
line_init ()
{
    line_clear ();
    line_insert ();
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

char
line_move_down ()
{
    if (linenr == num_lines - 1)
        return FALSE;

    set_current_line (++linenr);

    if (ypos != 23) {
        ypos++;
        term_put (TERM_LINE_FEED);
    }

    return TRUE;
}

void
line_open ()
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
print_linebuf ()
{
    linebuf[linebuf_length] = 0;
    term_puts (linebuf);
}

void
line_redraw ()
{
    disable_cursor ();
    set_cursor ();
    term_put (TERM_CARRIAGE_RETURN);

    print_linebuf ();

    term_put (TERM_CLEAR_TO_EOL);
    set_cursor ();
    enable_cursor ();
}

void
screen_redraw ()
{
    line * ls;
    char y;

    disable_cursor ();

    term_put (TERM_CLEAR_SCREEN);
    for (y = 0; y < 24; y++) {
        if (0) //y == linenr)
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

void
line_move_left ()
{
    if (xpos)
        xpos--;
}

void
line_move_right ()
{
    if (xpos < linebuf_length)
        xpos++;
}

void
line_insert_char (char c)
{
    linebuf_insert_char (xpos, c);
    line_move_right ();
}

void
line_delete_char ()
{
    if (!xpos)
        return;

    linebuf_delete_char (xpos);
    line_move_left ();
}

/*
line *
line_by_version (line * l, unsigned version)
{
    line  * m = &l->first;
    line  * n;

    if (m->version_deleted <= version || m->version > version)
        return NULL;

    while (n = m->newer) {
        if (n->version_deleted > version || n->version > version)
            break;

        m = n;
    }

    return m;
}

line *
line_get (unsigned i, unsigned version)
{
    line  * l = first_line;
    line       * m;

    do {
        m = line_by_version (l, version);
        if (m && !i--)
            return m;
    } while (l = l->next);

    return NULL;
}
*/

void
error (char * txt)
{
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (23);
    term_puts (txt);
    while (1);
}

void
line_test ()
{
    line * l;
    line * l2;
    line * l3;

    if (current_line != first_line)
        error ("Test 1");

    line_delete ();
    screen_redraw ();
    if (current_line)
        error ("Test 2");
    if (first_line)
        error ("Test 3");

    strcpy (linebuf, "foo");
    linebuf_length = strlen ("foo");
    line_insert ();
    screen_redraw ();
    if (current_line != first_line)
        error ("Test 4");
    if (!current_line)
        error ("Test 5");
    if (!first_line)
        error ("Test 6");
    if (num_lines != 1)
        error ("Test 7");
    l = current_line;

    strcpy (linebuf, "bar");
    linebuf_length = strlen ("bar");
    line_append ();
    screen_redraw ();
    if (first_line != l)
        error ("Test 8");
    if (!current_line)
        error ("Test 9");
    if (current_line == l)
        error ("Test 10");
    l2 = current_line;

    strcpy (linebuf, "baz");
    linebuf_length = strlen ("baz");
    line_append ();
    screen_redraw ();
    if (first_line != l)
        error ("Test 11");
    if (!current_line)
        error ("Test 12");
    if (current_line == l)
        error ("Test 13");
    if (current_line == l2)
        error ("Test 14");
    l3 = current_line;

    line_delete ();
    screen_redraw ();
    if (first_line != l)
        error ("Test 15");
    if (current_line)
        error ("Test 16");

    strcpy (linebuf, "bla");
    linebuf_length = strlen ("bla");
    line_append ();
    screen_redraw ();
    if (first_line != l)
        error ("Test 17");
    if (!current_line)
        error ("Test 18");
    if (current_line == l)
        error ("Test 19");
    if (current_line == l2)
        error ("Test 20");
}
