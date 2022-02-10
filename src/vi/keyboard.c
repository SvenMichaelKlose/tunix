#include <ctype.h>

#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>

#include "keyboard.h"

#define FALSE   0
#define TRUE    1

char       keylog[256];
unsigned   keylog_index;
unsigned   playback_index;
char       peeked_char;
char       does_log_keys;
char       does_play_back;

char
wait_for_key (void)
{
    char c;

    if (does_play_back) {
        if (playback_index == keylog_index)
            playback_index = 0;

        return keylog[playback_index++];
    }

    while (!(c = term_get ()));

    return c;
}

void
log_key (char c)
{
    if (does_log_keys)
        keylog[keylog_index++] = c;
}

void
unlog_key ()
{
    if (does_log_keys && keylog_index)
        keylog_index--;
}

char
get_key ()
{
    char c = peeked_char ?
        peeked_char :
        wait_for_key ();

    if (!peeked_char && does_log_keys && !does_play_back) {
        if (keylog_index == sizeof (keylog)) {
            // TODO: Tell in status line that
            // command cannot be repeated.
            term_put (TERM_BELL);
            keyboard_init ();
            does_log_keys = FALSE;
            return c;
        }

        log_key (c);
    }

    peeked_char = 0;
    return c;
}

char
peek_key ()
{
    if (peeked_char)
        return peeked_char;
    return peeked_char = get_key ();
}

void
start_playback ()
{
    does_play_back = TRUE;
    playback_index = 0;
}

void
stop_playback ()
{
    does_play_back = FALSE;
}

char
has_logged_keys ()
{
    return !!keylog_index;
}

void
reset_log ()
{
    keylog_index = 0;
}

void
keyboard_init ()
{
    reset_log ();
    playback_index = peeked_char = 0;
    does_play_back = FALSE;
    does_log_keys = TRUE;
}
