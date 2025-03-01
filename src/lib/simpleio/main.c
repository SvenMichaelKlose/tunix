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
#include <string.h>

#include "libsimpleio.h"

char       last_in[MAX_CHANNELS];
char       last_out[MAX_CHANNELS];
char       do_putback[MAX_CHANNELS];
char       putback_chars[MAX_CHANNELS];
simpleio * drivers[MAX_CHANNELS];

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
simpleio_chn_t  fnin;
simpleio_chn_t  fnout;
simpleio *      vin;
simpleio *      vout;
#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("vin")
#pragma zpsym ("vout")
#pragma bss-name (pop)
#endif

void FASTCALL
simpleio_close (simpleio_chn_t c)
{
    drivers[c]->close (c);
}

void FASTCALL
setin (simpleio_chn_t c)
{
    if (fnin != c) {
        fnin = c;
        vin = drivers[fnin];
        vin->setin (c);
    }
}

void FASTCALL
setout (simpleio_chn_t c)
{
    if (fnout != c) {
        fnout = c;
        vout = drivers[fnout];
        vout->setout (c);
    }
}

bool
eof ()
{
    return vout->eof ();
}

signed char
err ()
{
    return vout->err ();
}

char
_getold (void)
{
    do_putback[fnin] = false;
    return last_in[fnin] = putback_chars[fnin];
}

char
conin ()
{
    if (!do_putback[fnin])
        return last_in[fnin] = vin->conin ();
    return _getold ();
}

char
in ()
{
    if (!do_putback[fnin])
        return last_in[fnin] = vin->in ();
    return _getold ();
}

void FASTCALL
putbackc (char c)
{
    if (!eof ()) {
        do_putback[fnin] = true;
        putback_chars[fnin] = c;
    }
}

void
putback ()
{
    putbackc (last_in[fnin]);
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
    vout->out (c);
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

void
terpri (void)
{
    if (fnout != STDOUT) {
        out ('\n');
        return;
    }
#ifdef TARGET_C128
    out ('\r');
#else
    #ifdef __CC65__
        outs ("\n\r");
    #else
        out ('\n');
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
simpleio_init_channel (simpleio_chn_t chn, simpleio * s)
{
    do_putback[chn]    = false;
    putback_chars[chn] = 0;
    last_in[chn]       = 0;
    last_out[chn]      = 0;
    drivers[chn]       = s;
}

void
simpleio_clear_channels ()
{
    memset (do_putback, 0, sizeof (do_putback));
    memset (last_in,    0, sizeof (last_in));
    memset (last_out,   0, sizeof (last_out));
}

void
simpleio_init_common ()
{
    fnin  = STDIN;
    fnout = STDOUT;
    vin   = drivers[fnin];
    vout  = drivers[fnout];
}
