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

#include "libsimpleio.h"

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
simpleio * io;
simpleio_chn_t fnin;
simpleio_chn_t fnout;
char do_putback;
char last_in;
char last_out;
#ifdef __CC65__
#pragma zpsym ("io")
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("do_putback")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

void
simpleio_close (simpleio_chn_t c)
{
    io->close (c);
}

void
setin (simpleio_chn_t c)
{
    if (fnin != c) {
        fnin = c;
        io->setin (c);
    }
}

void
setout (simpleio_chn_t c)
{
    if (fnout != c) {
        fnout = c;
        last_out = 0;
        io->setout (c);
    }
}

char
eof ()
{
    return io->eof ();
}

char
err ()
{
    return io->err ();
}

char
in ()
{
    if (do_putback)
        do_putback = false;
    else
        last_in = io->in ();
    return last_in;
}

void
putback ()
{
    do_putback = true;
}

void
skip_spaces ()
{
    while (!eof ()) {
        // Skip comment until end of line.
        if (in () == ';') {
            while (!eof () && in () >= ' ');
            putback ();
        } else if (!isspace (last_in)) {
            putback ();
            return;
        }
    }
}

void
out (char c)
{
    last_out = c;
    io->out (c);
}

void
outnu (unsigned long n)
{
    if (n > 9)
        outnu (n / 10);
    out ('0' + n % 10);
}

void
outn (long n)
{
    if (n < 0) {
        out ('-');
        n = -n;
    }
    outnu (n);
}

void
outs (char * s)
{
    char c;
    while ((c = *s++))
        out (c);
}

void
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

void
fresh_line (void)
{
    if (last_out >= ' ')
        terpri ();
}

void
simpleio_set (simpleio * x)
{
    io = x;
    do_putback = false;
}
