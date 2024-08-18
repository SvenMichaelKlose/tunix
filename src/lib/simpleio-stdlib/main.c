#ifdef __CC65__

#ifndef __CBM__
#define __CBM__
#endif

#include <cbm.h>
#include <ingle/cc65-charmap.h>

#endif // #ifdef __CC65__

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#ifdef TARGET_UNIX
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#endif

#include <simpleio/libsimpleio.h>

#define MIN_CHANNEL     (STDERR + 1)
#define MAX_CHANNELS    256

FILE *      channels[MAX_CHANNELS];
signed char last_error;

#ifdef TARGET_UNIX

void
set_nonblocking_mode ()
{
    struct termios term;

    // Get current terminal attributes.
    tcgetattr (STDIN_FILENO, &term);

    // Set terminal to non-canonical mode and disable echo.
    term.c_lflag &= ~(ICANON | ECHO);

    // Set minimum number of characters for non-canonical read to return.
    term.c_cc[VMIN] = 1;
    term.c_cc[VTIME] = 0;

    // Apply the new settings.
    tcsetattr (STDIN_FILENO, TCSANOW, &term);

    // Set stdin to non-blocking.
    fcntl (STDIN_FILENO, F_SETFL, O_NONBLOCK);
}

void
reset_terminal_mode ()
{
    struct termios term;

    // Get current terminal attributes.
    tcgetattr (STDIN_FILENO, &term);

    // Reset terminal to canonical mode and enable echo.
    term.c_lflag |= (ICANON | ECHO);

    // Apply the reset settings.
    tcsetattr (STDIN_FILENO, TCSANOW, &term);

    // Reset stdin to blocking mode.
    fcntl (STDIN_FILENO, F_SETFL, 0);
}

#endif // #ifdef TARGET_UNIX

bool
raw_eof (void)
{
    return feof (channels[(int) fnin]);
}

signed char
raw_err (void)
{
    return last_error;
}

char
raw_in (void)
{
    int c;

    last_error = 0;
    c = fgetc (channels[(int) fnin]);
    if (c == EOF) {
        c = 0;
        if (errno)
            last_error = errno;
    }
    return c;
}

#ifdef TARGET_UNIX

char
raw_conin (void)
{
    int c;

    set_nonblocking_mode ();
    c = raw_in ();
    reset_terminal_mode ();

    return c;
}

#endif

void
raw_out (char c)
{
    last_error = 0;
    if (!channels[(int) fnout])
        last_error = -1;
    else if (EOF == fputc (c, channels[(int) fnout]))
        last_error = errno;
}

void
raw_setin (simpleio_chn_t c)
{
    (void) c;
}

void
raw_setout (simpleio_chn_t c)
{
    (void) c;
}

simpleio_chn_t
alloc_channel (FILE * handle)
{
    int i;

    for (i = MIN_CHANNEL; i < MAX_CHANNELS; i++)
        if (!channels[i]) {
            channels[i] = handle;
            return i;
        }

    return 0;
}

void
raw_close (simpleio_chn_t c)
{
    fclose (channels[(int) c]);
    channels[(int) c] = NULL;
}

simpleio_chn_t 
simpleio_open (char * name, char mode)
{
    FILE * handle;
    char m[2];

    last_error = 0;
    m[0] = mode;
    m[1] = 0;
    if ((handle = fopen (name, m)))
        return alloc_channel (handle);
    return 0;
}

simpleio vectors = {
    raw_eof,
    raw_err,
#ifdef TARGET_UNIX
    raw_conin,
#else
    raw_in,
#endif // #ifdef TARGET_UNIX
    raw_in,
    raw_out,
    raw_setin,
    raw_setout,
    raw_close
};

void
simpleio_init ()
{
    simpleio_set (&vectors);
    channels[STDIN]  = stdin;
    channels[STDOUT] = stdout;
    channels[STDERR] = stderr;
}
