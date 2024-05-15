#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <cbm.h>

#include <ingle/cc65-charmap.h>
#include <term/libterm.h>
#include <lineedit/liblineedit.h>
#include <lineedit/linebuf.h>
#include <text/line.h>
#include <text/motion.h>

#include "commands.h"
#include "screen.h"
#include "keyboard.h"


char * filename = NULL;

char * passphrase = NULL;
unsigned char passphrase_length;
unsigned char passphrase_index;

void
wait_keypress (void)
{
    term_puts ("\n\rPress any key to continue...");
    get_key ();
    gotoxy (0, rows - 2);
    term_put (TERM_CLEAR_TO_EOL);
    screen_set_status ("");
    screen_redraw ();
}

void
cmd_open_above ()
{
    line_insert_before ();
    move_line_start ();

    changes_first = linenr;
    changes_last = 32000;
}

void
cmd_open_below ()
{
    line_insert_after ();
    move_down ();
    move_line_start ();

    changes_first = linenr;
    changes_last = 32000;
}

void
cmd_enter ()
{
    line * current_line = line_get (linenr);

    if (xpos == current_line->length)
        line_insert_after ();
    else
        line_split ();
    move_down ();
    move_line_start ();

    changes_first = linenr - 1;
    changes_last = 32000;
}

void
cmd_delete_line ()
{
    line_delete ();
    move_line_start ();

    changes_first = linenr;
    changes_last = 32000;
}

void
cmd_delete_till_line_end ()
{
    line * current_line = line_get (linenr);

    current_line->length = xpos;
    if (xpos)
        xpos--;

    changes_first = linenr;
    changes_last = linenr;
}

void
cmd_change_till_line_end ()
{
    cmd_delete_till_line_end ();
    move_line_end ();

    changes_first = linenr;
    changes_last = linenr;
}

void
cmd_delete_char ()
{
    line_to_buf ();
    linebuf_delete_char (xpos);
    buf_to_line ();
    adjust_xpos_to_line_length ();

    changes_first = linenr;
    changes_last = linenr;
}

void
cmd_replace_char ()
{
    char c = get_key ();

    if (c < 32) {
        term_put (TERM_BELL);
        keyboard_init ();
        return;
    }

    line_to_buf ();
    linebuf_replace_char (xpos, c);
    buf_to_line ();

    changes_first = linenr;
    changes_last = linenr;
}

void
cmd_join ()
{
    line * current_line = line_get (linenr);
    unsigned len = current_line->length;

    if (!current_line->next)
        return;

    line_join ();
    xpos = len;

    changes_first = linenr;
    changes_last = 32000;
}

void
cmd_set_passphrase ()
{
    gotoxy (0, 23);

    if (passphrase) {
        free (passphrase);
        passphrase = NULL;
    }

    if (linebuf_length == 2) {
        term_puts ("Passphrase deleted.");
        wait_keypress ();
        return;
    }

    passphrase_length = linebuf_length - 2;
    passphrase = malloc (passphrase_length);
    memcpy (passphrase, &linebuf[2], linebuf_length);
    term_puts ("Passphrase set.");
    term_put (TERM_CLEAR_TO_EOL);
    wait_keypress ();
}

char
encrypt (char c)
{
    if (passphrase) {
        c ^= passphrase[passphrase_index++];
        passphrase_index %= passphrase_length;
    }

    return c;
}

void
set_filename (char * s, int len)
{
    if (filename)
        free (filename);
    filename = malloc (len + 1);
    memcpy (filename, s, len);
}

void
cmd_write_file ()
{
    line *    l = first_line;
    char *    fn;
    char      err;
    unsigned  i;

    linebuf[linebuf_length] = 0;
    passphrase_index = 0;

    if (linebuf_length == 2 && !filename) {
        gotoxy (0, 23);
        term_puts ("File name missing.");
        wait_keypress ();
        return;
    } else if (linebuf_length > 2)
        set_filename (&linebuf[2], linebuf_length);

    fn = malloc (strlen (filename) + 9);
    sprintf (fn, "@:%S,S,W", filename);
    err = cbm_open (3, 8, 3, fn);
    free (fn);
    if (err) {
        gotoxy (0, 23);
        term_puts ("Cannot write file '");
        term_puts (fn);
        term_puts ("'.\n\r");
        wait_keypress ();
        return;
    }

    screen_set_status ("Writing...");
    cbm_k_ckout (3);
    while (l) {
        for (i = 0; i < l->length; i++)
            cbm_k_bsout (encrypt (l->data[i]));
        cbm_k_bsout (encrypt (10));

        l = l->next;
    }

    cbm_close (3);

    gotoxy (0, 22);
    term_puts ("Wrote ");
    if (passphrase)
        term_puts ("ENCRYPTED ");
    term_puts ("file '");
    term_puts (filename);
    term_puts ("'.");
    wait_keypress ();
}

void
cmd_read_file ()
{
    char *  data = malloc (256);
    char *  p;
    line *  current_line;
    line *  l;
    int     len;
    char    c;
    char    err;

    linebuf[linebuf_length] = 0;
    passphrase_index = 0;

    gotoxy (0, 23);
    if (err = cbm_open (2, 8, 2, &linebuf[2])) {
        term_puts ("Cannot open file '");
        term_puts (&linebuf[2]);
        term_puts ("'.\n\r");
        wait_keypress ();
        goto done;
    }
    set_filename (&linebuf[2], linebuf_length);

    line_clear ();
    current_line = first_line = NULL;
    num_lines = 0;

    screen_set_status ("Reading...");
    cbm_k_chkin (2);
    while (!cbm_k_readst ()) {
        p = data;
        while (1) {
            c = encrypt (cbm_k_basin ());
            if (cbm_k_readst () || c == 10)
                break;
            if (c == 13)
                continue;

            *p++ = c;
        }
        len = p - data;

        l = line_alloc ();
        if (!first_line)
            first_line = l;
        l->data = malloc (len);
        l->length = len;
        memcpy (l->data, data, len);

        if (current_line) {
            current_line->next = l;
            l->prev = current_line;
        }
        current_line = l;
        num_lines++;
    }

done:
    cbm_close (2);
    free (data);
    screen_set_status ("");
    screen_redraw ();
}

//
// unfinished
//

void cmd_delete ()
{
}

void cmd_change ()
{
}

void cmd_yank ()
{
}

void cmd_paste ()
{
}

void cmd_follow ()
{
}

void cmd_paste_above ()
{
}

void cmd_paste_below ()
{
}

void cmd_toggle_visual_mode ()
{
}

void cmd_quit ()
{
}
