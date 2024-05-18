#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include "libsimpleio.h"

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
char      c;
char      do_putback;
#ifdef __CC65__
#pragma zpsym ("c")
#pragma zpsym ("do_putback")
#pragma bss-name (pop)
#endif

char
raw_eof ()
{
    return cbm_k_readst () & 0x40;
}

char
raw_in ()
{
    return cbm_k_basin ();
}

void
raw_out (char c)
{
    cbm_k_bsout (c);
}

void
raw_start_error ()
{
    cbm_k_ckout (3);
}

char
eof ()
{
    return raw_eof ();
}

char
in ()
{
    if (do_putback) {
        do_putback = false;
        return c;
    }
    c = raw_in ();
    return c;
}

char
ch ()
{
    return c;
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
        if (in () == ';') {
            while (!eof () && in () >= ' ');
            while (!eof () && in () < ' ');
            putback ();
            continue;
        }
        if (!isspace (c)) {
            putback ();
            return;
        }
    }
}

void
out (char c)
{
    raw_out (c);
}

void
out_number (int n)
{
    int a;
    if (n > 9) {
        a = n / 10;
        n -= 10 * a;
        out_number (a);
    }
    out ('0' + n);
}

void
outs (char * s)
{
    char c;
    while (c = *s++)
        out (c);
}

void
outsn (char * s, char len)
{
    while (len--)
        out (*s++);
}

void
errouts (char * str)
{
    raw_start_error ();
    outs ("ERROR: ");
    outs (str);
}
