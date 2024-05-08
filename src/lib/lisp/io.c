#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <term/libterm.h>

#include "liblisp.h"

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

void
error (char * str)
{
    term_puts ("ERROR: ");
    term_puts (str);
    while (1);
}

char
eof ()
{
    return cbm_k_readst () & 0x40;
}

char
in ()
{
    if (do_putback) {
        do_putback = false;
        return c;
    }
    c = cbm_k_basin ();
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
            while (!eof ()
                   && in () >= ' ');
            while (!eof ()
                   && in () < ' ');
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
    term_put (c);
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
    term_puts (s);
}

void
outsn (char * s, char len)
{
    term_putsn (s, len);
}
