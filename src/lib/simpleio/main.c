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

simpleio *      io;
simpleio_chn_t  fnin;
simpleio_chn_t  fnout;
char            do_putback;
char            last_in;
char            last_out;

#ifdef __CC65__
#pragma zpsym ("io")
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("do_putback")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
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
        last_out = 0;
        io->setout (c);
    }
}

bool
eof ()
{
    if (do_putback)
        return false;
    return io->eof ();
}

signed char
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

void FASTCALL
out (char c)
{
    last_out = c;
    io->out (c);
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
        n = abs (n);
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

void
fresh_line (void)
{
    if (last_out >= ' ')
        terpri ();
}

void FASTCALL
simpleio_set (simpleio * x)
{
    io = x;
    do_putback = false;
}
