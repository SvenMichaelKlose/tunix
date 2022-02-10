#include <ctype.h>
#include <stdlib.h>

#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
#include "screen.h"
#include "keyboard.h"

#define CANCELLED   -1
#define OK          0

typedef void (*voidfun) ();

void do_nothing (void) {}

typedef struct _command {
    char     key;
    voidfun  fun;
} command;

// After these edit_mode() is called.
command edit_commands[] = {
    { 'i', do_nothing },
    { 'I', move_line_begin },
    { 'o', cmd_open_below },
    { 'O', cmd_open_above },
    { 'a', move_right },
    { 'A', move_line_end },
    { 'C', cmd_change_till_line_end },
    { 's', cmd_delete_char },
    { 0, NULL }
};

command motion_commands[] = {
    { TTY_CURSOR_LEFT,  move_left },
    { TTY_CURSOR_UP,    move_up },
    { TTY_CURSOR_DOWN,  move_down },
    { TTY_CURSOR_RIGHT, move_right },
    { 'h', move_left },
    { 'k', move_up },
    { 'j', move_down },
    { 'l', move_right },
    { '0', move_line_start },
    { '$', move_line_last_char },
    { 0, NULL }
};

command modify_commands[] = {
    { 'D', cmd_delete_till_line_end },
    { 'x', cmd_delete_char },
    { 'r', cmd_replace_char },
    { 'J', cmd_join },
    { 0, NULL }
};

command complex_commands[] = {
    { 'w', cmd_write_file },
    { 'r', cmd_read_file },
    { 'k', cmd_set_passphrase },
    { 0, NULL }
};

char
input (void)
{
    char key;

    while (1) {
        switch (key = get_key ()) {
            case TTY_ENTER:
            case TTY_ESCAPE:
                goto done;

            default:
                if (key == TTY_BACKSPACE && !xpos)
                    goto done;
                lineedit (key);
        }
    }

done:
    return key;
}

void
edit_mode (void)
{
    char c;
    screen_set_status ("-- INSERT --");

    while (1) {
        screen_redraw ();

        linelist_goto (linenr);
        linelist_line_to_buf ();
        c = input ();
        buf_to_linelist ();

        switch (c) {
            case TTY_ENTER:
                cmd_enter ();
                continue;

            case TTY_ESCAPE:
                goto done;

            case TTY_BACKSPACE:
                if (linenr) {
                    move_up ();
                    cmd_join ();
                }
                continue;
        }
    }

done:
    move_left ();
    screen_set_status ("");
}

voidfun
get_command_fun (command * cmds, char c)
{
    while (cmds->key) {
        if (cmds->key == c)
            return cmds->fun;
        cmds++;
    }

    return NULL;
}

char
exec_single_command ()
{
    char     c;
    voidfun  fun;

    c = get_key ();

    if (fun = get_command_fun (edit_commands, c)) {
        fun ();
        edit_mode ();
    } else if (fun = get_command_fun (motion_commands, c))
        fun ();
    else if (fun = get_command_fun (modify_commands, c))
        fun ();
    else
        return c;

    return OK;
}

unsigned
get_repetitions (void)
{
    char      c;
    unsigned  n = 0;

    while (1) {
        c = peek_key ();

        if (c == TTY_ESCAPE) {
            get_key ();
            return 0;
        }

        if (!isdigit (c))
            return n;

        if (n > 6553) {
            term_put (TERM_BELL);
            return 0;
        }

        get_key ();
        unlog_key ();
        n *= 10;
        n += c - '0';
    }
}

char exec_action (void);

void
playback (void)
{
    start_playback ();
    exec_action ();
    stop_playback ();
}

// Do an action with optional number of repitions prefixed.
char
exec_action ()
{
    unsigned  repetitions = 0;

    linelist_goto (linenr);

    if (peek_key () != '0')
        repetitions = get_repetitions ();

    if (exec_single_command ())
        goto cancel;

    if (repetitions) {
        repetitions--;  // Not very happy about this somehow. (pixel)
        while (repetitions--)
            playback ();
    }

    return OK;

cancel:
    term_put (TERM_BELL);
    return CANCELLED;
}


void
exec_complex (void)
{
    unsigned  oldx = xpos;
    char      c;
    voidfun   f;

    lineedit_init ();
    ypos = 23;  // TODO: Fetch dynamic height.
    c = input ();

    if (c == TTY_ENTER) {
        f = get_command_fun (complex_commands, linebuf[1]);
        if (f)
            f ();
    }

    xpos = oldx;
}

// Top level controlling keyboard logging
// and triggering playback.
void
toplevel (void)
{
    char c;

    while (1) {
        c = peek_key ();

        if (c == ':')
            exec_complex ();
        else if (has_logged_keys () && c == '.') {
            get_key ();
            unlog_key ();
            playback ();
        } else if (exec_action ())
            keyboard_init ();

        screen_redraw ();
    }
}

void
main (void)
{
    // TODO: Check if memory expansion is there.
    _heapadd ((void *) 0x400, 0xc00);   /* +3K */
    //_heapadd ((void *) 0x9800, 0x800);  /* IO2,3 */
    _heapadd ((void *) 0xa000, 0x2000); /* BANK5 */

    term_init ();
    linelist_init ();
    screen_init ();
    keyboard_init ();
    screen_redraw ();
    toplevel ();
}
