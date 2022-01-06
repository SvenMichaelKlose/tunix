#include <ctype.h>

#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
#include "screen.h"
#include "keyboard.h"

typedef void (*voidfun) ();

void do_nothing (void) {}

typedef struct _command {
    char        key;
    voidfun     fun;
} command;

command edit_commands[] = {
    { 'i', do_nothing },
    { 'I', move_line_begin },
    { 'o', cmd_open_below },
    { 'O', cmd_open_above },
    { 'a', move_right },
    { 'A', move_line_end },
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
    { 'J', cmd_join },
    { 0, NULL }
};

char
lineedit_mode (void)
{
    char key;

    linelist_goto (linenr);
    linelist_line_to_buf ();

    while (1) {
        switch (key = get_key ()) {
            case TTY_ENTER:
            case TTY_ESCAPE:
                goto done;

            default:
                lineedit (key);
        }
    }

done:
    linelist_buf_to_line ();

    return key;
}

void
edit_mode (void)
{
    char key;

    screen_set_status ("-- INSERT --");

    while (1) {
        screen_redraw ();

        switch (key = lineedit_mode ()) {
            case TTY_ENTER:
                cmd_enter ();
                continue;

            case TTY_ESCAPE:
                goto done;
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
    char     c = get_key ();
    voidfun  fun;

    if (fun = get_command_fun (edit_commands, c)) {
        fun ();
        edit_mode ();
    } else if (fun = get_command_fun (motion_commands, c))
        fun ();
    else if (fun = get_command_fun (modify_commands, c))
        fun ();
    else
        return c;

    return 0;
}

void
playback (void)
{
    is_playback = TRUE;
    exec_single_command ();
    is_playback = FALSE;
}

unsigned
get_prefix (void)
{
    char      c;
    unsigned  n = 0;

    while (1) {
        if (n > 6553) {
            term_put (TERM_BELL);
            return 0;
        }

        c = peek_key ();

        if (c == TTY_ESCAPE) {
            get_key ();
            keyboard_init ();
            return 0;
        }

        if (!isdigit (c))
            return n;

        get_key ();
        n *= 10;
        n += c - '0';
    }
}

void
command_mode (void)
{
    char      key;
    unsigned  n;

    while (1) {
        n = 0;

        linelist_goto (linenr);

        key = peek_key ();

        if (key == '.') {
            get_key ();
            unlog_key ();
            playback ();
            goto next;
        }

        if (isdigit (key)) {
            n = get_prefix ();
            if (!n)
                continue;
        }

        if (key = exec_single_command ()) {
            term_put (TERM_BELL);
            keyboard_init ();
            continue;
        }

next:
        screen_redraw ();

        while (n--)
            playback ();
    }
}

int
main (void)
{
    term_init ();
    linelist_init ();
    screen_init ();
    keyboard_init ();
    screen_redraw ();
    command_mode ();
}
