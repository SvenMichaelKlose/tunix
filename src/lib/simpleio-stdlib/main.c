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
#include <simpleio/control.h>

#define MIN_CHANNEL     (STDERR + 1)

FILE *      channels[MAX_CHANNELS];
signed char last_error;
char        con_flags;

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

void
cmd_null (void)
{
}

void
cmd_goto (void)
{
    fputs ("\033[", stdout);
    outn (cmd_params[0]);
    fputc (';', stdout);
    outn (cmd_params[1]);
    fputc ('H', stdout);
    fflush (stdout);
}

void
cmd_clr (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR) {
        con_flags &= ~TERM_FLAG_CURSOR;
        fputs ("\033[?25l", stdout);
    }
    if (c & TERM_FLAG_REVERSE) {
        con_flags &= ~TERM_FLAG_REVERSE;
        fputs ("\033[27m", stdout);
    }
#ifdef TARGET_UNIX
    if (c & TERM_FLAG_DIRECT) {
        con_flags &= ~TERM_FLAG_DIRECT;
        reset_terminal_mode ();
    }
#endif
    fflush (stdout);
}

void
cmd_set (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR) {
        con_flags |= TERM_FLAG_CURSOR;
        fputs ("\033[?25h", stdout);
    }
    if (c & TERM_FLAG_REVERSE) {
        con_flags |= TERM_FLAG_REVERSE;
        fputs ("\033[7m", stdout);
    }
#ifdef TARGET_UNIX
    if (c & TERM_FLAG_DIRECT) {
        con_flags |= TERM_FLAG_DIRECT;
        set_nonblocking_mode ();
    }
#endif
    fflush (stdout);
}

void
cmd_get (void)
{
    putbackc (con_flags);
}

void
cmd_clrscr (void)
{
    fputs ("\033[2J\033[H", stdout);
    fflush (stdout);
}

void
cmd_lf (void)
{
    fputc (10, stdout);
}

void
cmd_cr (void)
{
    fputc (13, stdout);
}

#ifndef __CC65__

// Get terminal cursor position.
// Returns 'false' on error.
bool
getxy (int * col, int * row)
{
#ifdef TARGET_UNIX
    int old_c_lflag;
    struct termios term;

    // Disable terminal's canonical mode and echo.
    if (tcgetattr (STDIN_FILENO, &term))
        return false;
    old_c_lflag = term.c_lflag;
    term.c_lflag &= ~(ICANON | ECHO);
    if (tcsetattr (STDIN_FILENO, TCSANOW, &term))
        return false;
    term.c_lflag = old_c_lflag;

    // Ensure standard input file number is in blocking mode.
    int flags = fcntl (STDIN_FILENO, F_GETFL, 0);
    if (flags & O_NONBLOCK)
        fcntl (STDIN_FILENO, F_SETFL, flags & ~O_NONBLOCK); // Clear O_NONBLOCK

    // Request cursor position.
    if (4 != write (STDOUT_FILENO, "\033[6n", 4))
        goto error_with_term_restored;

    // Read the response.
    char buf[32] = {0};
    if (0 >= read (STDIN_FILENO, buf, sizeof (buf) - 1))
        goto error_with_term_restored;

    // Scan the response.
    if (2 != sscanf (buf, "\033[%d;%dR", row, col))
        goto error_with_term_restored;

    // Restore former terminal status.
    if (tcsetattr (STDIN_FILENO, TCSANOW, &term))
        return false;
#endif

    return true;

error_with_term_restored:
#ifdef TARGET_UNIX
    fcntl (STDIN_FILENO, F_SETFL, flags);
    tcsetattr (STDIN_FILENO, TCSANOW, &term);
#endif
    return false;
}

#endif // #ifndef __CC65__

void
cmd_getx (void)
{
#ifdef __CC65__
    putbackc (0);
#else
    int row, col;
    getxy (&row, &col);
    putbackc (row);
#endif // #ifdef __CC65__
}

void
cmd_gety (void)
{
#ifdef __CC65__
    putbackc (0);
#else
    int row, col;
    getxy (&row, &col);
    putbackc (col);
#endif // #ifdef __CC65__
}

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
#ifdef __CC65__
        if (errno && errno != EAGAIN)
#else
        if (errno && errno != EWOULDBLOCK && errno != EAGAIN)
#endif
            last_error = errno;
    }
    return c;
}

#ifdef TARGET_UNIX

char
raw_conin (void)
{
    int c;

    if (!(con_flags & TERM_FLAG_DIRECT))
        set_nonblocking_mode ();
    c = raw_in ();
    if (!(con_flags & TERM_FLAG_DIRECT))
        reset_terminal_mode ();

    return c;
}

#endif

void
raw_out (char c)
{
    last_error = 0;
    if (simpleio_control (c))
        return;
    if (!channels[(int) fnout])
        last_error = -1;
    else if (EOF == fputc (c, channels[(int) fnout]))
        last_error = errno;
    if (fnout == STDOUT && (con_flags & TERM_FLAG_DIRECT))
        fflush (stdout);
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

simpleio_chn_t 
simpleio_open (char * name, char mode)
{
    FILE * handle;
    char m[2];
    simpleio_chn_t chn;

    last_error = 0;
    m[0] = mode;
    m[1] = 0;
    if ((handle = fopen (name, m))) {
        chn = alloc_channel (handle);
        simpleio_init_channel (chn, &vectors);
        return chn;
    }
    return 0;
}

void
simpleio_init ()
{
    channels[STDIN]  = stdin;
    channels[STDOUT] = stdout;
    channels[STDERR] = stderr;
    simpleio_clear_channels ();
    simpleio_init_channel (STDIN,  &vectors);
    simpleio_init_channel (STDOUT, &vectors);
    simpleio_init_channel (STDERR, &vectors);
    simpleio_init_common ();
    con_flags = TERM_FLAG_CURSOR;
}
