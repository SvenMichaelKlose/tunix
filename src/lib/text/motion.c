#include <lib/ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <lib/lineedit/liblineedit.h>
#include <lib/term/libterm.h>

#include "line.h"
#include "motion.h"

void
adjust_xpos_to_line_length ()
{
    line * current_line = line_get (linenr);

    if (xpos >= current_line->length)
        xpos = current_line->length ?
            current_line->length - 1 :
            0;
}

char
current_char (void)
{
    return line_get (linenr)->data[xpos];
}

bool
is_last_line ()
{
    return linenr == num_lines - 1;
}

bool
is_line_start ()
{
    return !xpos;
}

bool
is_line_end ()
{
    line * current_line = line_get (linenr);
    unsigned l = current_line->length;
    return xpos == l;
}

void
move_up ()
{
    if (linenr)
        --linenr;
    adjust_xpos_to_line_length ();
}

void
move_down ()
{
    if (!is_last_line ())
        linenr++;
    adjust_xpos_to_line_length ();
}

void
move_left ()
{
    if (xpos)
        xpos--;
}

void
move_right ()
{
    if (!is_line_end ())
        xpos++;
}

void
move_line_start ()
{
    xpos = 0;
}

void
move_line_end ()
{
    line * current_line = line_get (linenr);
    xpos = current_line->length;
}

void
move_line_begin ()
{
    line * current_line = line_get (linenr);

    if (!current_line->length)
        return;

    xpos = 0;
    while (current_char () == ' ' && !is_line_end ())
        move_right ();
}

void
move_line_last_char ()
{
    line * current_line = line_get (linenr);
    if (current_line->length)
        xpos = current_line->length - 1;
}

void
move_last_line ()
{
    linenr = num_lines - 1;
}

void
move_next_line_begin (void)
{
    move_down ();
    move_line_begin ();
}

void
move_prev_line_end (void)
{
    move_up ();
    move_line_end ();
}

bool
step_char_forwards (void)
{
    move_right ();
    if (is_line_end ()) {
        if (is_last_line ())
            return false;
        move_next_line_begin ();
    }

    return true;
}

void
move_word ()
{
    do {
        if (is_line_end ()) {
            move_next_line_begin ();
            return;
        }
        move_right ();
    } while (current_char () != ' ');
    do {
        if (is_line_end ()) {
            move_next_line_begin ();
            return;
        }
        move_right ();
    } while (current_char () == ' ');
}

void
move_word_back ()
{
    do {
        if (is_line_start ()) {
            move_prev_line_end ();
            return;
        }
        move_left ();
    } while (current_char () != ' ');
    do {
        if (is_line_start ()) {
            move_prev_line_end ();
            return;
        }
        move_left ();
    } while (current_char () == ' ');
}
