#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/lineedit/linebuf.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
#include "screen.h"
#include "keyboard.h"


char * filename = NULL;

char * passphrase = NULL;
unsigned char passphrase_length;
unsigned char passphrase_index;

void
wait4user (void)
{
    term_puts ("\n\rPress any key to continue...");
    get_key ();
}

void
cmd_open_above ()
{
    linelist_insert_before ();
    move_line_start ();
}

void
cmd_open_below ()
{
    linelist_insert_after ();
    move_down ();
    move_line_start ();
}

void
cmd_enter ()
{
    if (xpos == current_line->length)
        linelist_insert_after ();
    else
        linelist_split ();
    move_down ();
    move_line_start ();
}

void
cmd_change_till_line_end ()
{
    cmd_delete_till_line_end ();
    move_line_end ();
}

void
cmd_delete_till_line_end ()
{
    current_line->length = xpos;
    if (xpos)
        xpos--;
}

void
cmd_delete_char ()
{
    linelist_line_to_buf ();
    linebuf_delete_char (xpos);
    buf_to_linelist ();
    adjust_xpos_to_line_length ();
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

    linelist_line_to_buf ();
    linebuf_replace_char (xpos, c);
    buf_to_linelist ();
}

void
cmd_join ()
{
    unsigned len = current_line->length;

    if (!current_line->next)
        return;

    linelist_join ();
    xpos = len;
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
        wait4user ();
        return;
    }

    passphrase_length = linebuf_length - 2;
    passphrase = malloc (passphrase_length);
    memcpy (passphrase, &linebuf[2], linebuf_length);
    term_puts ("Passphrase set.");
    term_put (TERM_CLEAR_TO_EOL);
    wait4user ();
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
cmd_write_file ()
{
    line * l = first_line;
    char err;
    unsigned i;
    char * fn;

    if (linebuf_length == 2 && !filename) {
        gotoxy (0, 23);
        term_puts ("File name missing.");
        wait4user ();
        return;
    }
    linebuf[linebuf_length] = 0;
    passphrase_index = 0;

    if (err = cbm_open (1, 8, 1, &linebuf[2])) {
        gotoxy (0, 23);
        term_puts ("Cannot open file '");
        term_puts (&linebuf[2]);
        term_puts ("'.\n\r");
        wait4user ();
        return;
    }

    cbm_k_ckout (1);
    while (l) {
        for (i = 0; i < l->length; i++)
            cbm_k_bsout (encrypt (l->data[i]));

        cbm_k_bsout (encrypt (10));
        l = l->next;
    }

    cbm_close (1);

    gotoxy (0, 23);
    term_puts ("Wrote ");
    if (passphrase)
        term_puts ("ENCRYPTED ");
    term_puts ("file '");
    term_puts (&linebuf[2]);
    term_puts ("'.");
    wait4user ();
}

void
cmd_read_file ()
{
    char *    data = malloc (1024);
    char *    p;
    line *    l;
    int       len;
    char      c;
    char      err;

    linebuf[linebuf_length] = 0;
    passphrase_index = 0;

    gotoxy (0, 23);
    if (err = cbm_open (2, 8, 2, &linebuf[2])) {
        term_puts ("Cannot open file '");
        term_puts (&linebuf[2]);
        term_puts ("'.\n\r");
        wait4user ();
        goto done;
    }

    linelist_clear ();
    free (first_line);
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

    current_line = first_line;

done:
    cbm_close (2);
    free (data);
    screen_set_status ("");
}
