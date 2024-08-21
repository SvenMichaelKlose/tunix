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
#ifdef __CC65__
#include <string.h>
#else
#include <strings.h>
#endif

#include "libsimpleio.h"

char    last_in[MAX_CHANNELS];
char    last_out[MAX_CHANNELS];
char    do_putback[MAX_CHANNELS];
char    putback_chars[MAX_CHANNELS];

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
simpleio *      io;
simpleio_chn_t  fnin;
simpleio_chn_t  fnout;
#ifdef __CC65__
#pragma zpsym ("io")
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma bss-name (pop)
#endif

void FASTCALL
simpleio_close (simpleio_chn_t c)
{
    io->close (c);
}

void FASTCALL
setin (simpleio_chn_t c)
{
    if (fnin != c) {
        fnin = c;
        io->setin (c);
    }
}

void FASTCALL
setout (simpleio_chn_t c)
{
    if (fnout != c) {
        fnout = c;
        io->setout (c);
    }
}

bool
eof ()
{
    if (do_putback[fnin])
        return false;
    return io->eof ();
}

signed char
err ()
{
    return io->err ();
}

char
conin ()
{
    if (!do_putback[fnin])
        return last_in[fnin] = io->conin ();
    do_putback[fnin] = false;
    return last_in[fnin] = putback_chars[fnin];
}

char
in ()
{
    if (!do_putback[fnin])
        return last_in[fnin] = io->in ();
    do_putback[fnin] = false;
    return last_in[fnin] = putback_chars[fnin];
}

void
putback ()
{
    if (!eof ()) {
        do_putback[fnin] = true;
        putback_chars[fnin] = last_in[fnin];
    }
}

char
lastin ()
{
    return last_in[fnin];
}

bool
skip_spaces ()
{
    char c;
    while (1) {
        c = in ();
        if (eof ())
            break;
        if (!isspace (c)) {
            putback ();
            return false;
        }
    }
    return true;
}

void FASTCALL
out (char c)
{
    last_out[fnout] = c;
    io->out (c);
}

char
lastout ()
{
    return last_out[fnout];
}

void FASTCALL
outnu (unsigned long n)
{
    if (n > 9)
        outnu (n / 10);
    out ('0' + n % 10);
}

void FASTCALL
outn (long n)
{
    if (n < 0) {
        out ('-');
        n = labs (n);
    }
    outnu (n);
}

void FASTCALL
outs (char * s)
{
    char c;
    while ((c = *s++))
        out (c);
}

void FASTCALL
outsn (char * s, char len)
{
    while (len--)
        out (*s++);
}

void
terpri (void)
{
#ifdef TARGET_C128
    outs ("\r");
#else
    #ifdef __CC65__
        outs ("\n\r");
    #else
        outs ("\n");
    #endif
#endif
}

char c;

void
fresh_line (void)
{
    c = last_out[fnout];
    if (c && c != '\n' && c != '\r')
        terpri ();
}

void FASTCALL
simpleio_init_channel (simpleio_chn_t chn)
{
    do_putback[chn] = false;
    putback_chars[chn] = 0;
    last_in[chn] = 0;
    last_out[chn] = 0;
}

void FASTCALL
simpleio_set (simpleio * x)
{
    io = x;
    bzero (do_putback, sizeof (do_putback));
    bzero (last_in, sizeof (last_in));
    bzero (last_out, sizeof (last_out));
}
