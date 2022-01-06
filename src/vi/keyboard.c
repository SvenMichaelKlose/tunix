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
char       is_keylog;
char       is_playback;

char
wait_for_key (void)
{
    char c;

    while (!(c = term_get ()));

    return c;
}

void
log_key (char c)
{
    keylog[keylog_index++] = c;
}

void
unlog_key ()
{
    if (keylog_index)
        keylog_index--;
}

char
get_key ()
{
    char c;

    if (is_playback) {
        if (playback_index == keylog_index)
            playback_index = 0;

        return keylog[playback_index++];
    }

    if (peeked_char) {
        c = peeked_char;
        peeked_char = 0;
    } else
        c = wait_for_key ();

    if (!is_keylog)
        return c;

    if (keylog_index == sizeof (keylog)) {
        term_put (TERM_BELL);
        // TODO: Tell that command cannot be repeated.
        keyboard_init ();
        is_keylog = FALSE;
        return c;
    }

    log_key (c);
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
    is_playback = TRUE;
    playback_index = 0;
}

void
stop_playback ()
{
    is_playback = FALSE;
}

void
keyboard_init ()
{
    keylog_index = playback_index = peeked_char = 0;
    is_playback = FALSE;
    is_keylog = TRUE;
}
