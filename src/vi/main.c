#include <ctype.h>

#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
#include "screen.h"

char
wait_for_key (void)
{
    char key;

    while (!(key = term_get ()));

    return key;
}

char
lineedit_mode (void)
{
    char key;

    linelist_goto (linenr);
    linelist_line_to_buf ();

    while (1) {
        switch (key = wait_for_key ()) {
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

typedef void (*voidfun) ();

typedef struct _command {
    char        key;
    voidfun     fun;
} command;

voidfun
get_command_fun (command * cmds, char key)
{
    while (cmds->key) {
        if (cmds->key == key)
            return cmds->fun;
        cmds++;
    }

    return NULL;
}

void do_nothing (void) {}

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
exec_single_command (char key)
{
    voidfun fun;

    if (fun = get_command_fun (edit_commands, key)) {
        fun ();
        edit_mode ();
    } else if (fun = get_command_fun (motion_commands, key))
        fun ();
    else if (fun = get_command_fun (modify_commands, key))
        fun ();
    else
        return key;

    return 0;
}

void
handle_prefixed (char key)
{
    unsigned n = 0;

    while (1) {
        if (n > (65535 / 10)) {
            term_put (TERM_BELL);
            return;
        }

        n *= 10;
        n += key - '0';

        key = wait_for_key ();
        if (isdigit (key))
            continue;

        while (n--)
            exec_single_command (key);
        return;

        // TODO to make this work with multi-key
        // commands:
        //
        // Clear key log.
        // Enter command mode.
        // Break if key log empty.
        // Copy key log.
        // Play key log back n-1 times.
    }
}

void
command_mode (void)
{
    char    key;

    while (1) {
        linelist_goto (linenr);

        key = exec_single_command (wait_for_key ());
        if (key) {
            if (isdigit (key))
                handle_prefixed (key);
            else {
                term_put (TERM_BELL);
                continue;
            }
        }

        screen_redraw ();
    }
}

int
main (void)
{
    term_init ();
    linelist_init ();
    screen_init ();
    screen_redraw ();
    command_mode ();
}
