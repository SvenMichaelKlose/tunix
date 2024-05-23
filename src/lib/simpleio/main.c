#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif
#include <cbm.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include "libsimpleio.h"

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
char c;
char do_putback;
char last_out;
#ifdef __CC65__
#pragma zpsym ("c")
#pragma zpsym ("do_putback")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

#define UPCASE_A  65
#define UPCASE_Z  90
#define LOCASE_A  97
#define LOCASE_Z 122
#define ISUPPER(c) (c >= UPCASE_A && c <= UPCASE_Z)
#define ISLOWER(c) (c >= LOCASE_A && c <= LOCASE_Z)
#define TOUPPER(c) (c - LOCASE_A + UPCASE_A)
#define TOLOWER(c) (c - UPCASE_A + LOCASE_A)

char
reverse_case (char c)
{
    if (ISUPPER(c))
        return TOLOWER(c);
    if (ISLOWER(c))
        return TOUPPER(c);
    return c;
}

char
raw_eof (void)
{
    return cbm_k_readst () & 0x40;
}

char
raw_in (void)
{
    return cbm_k_basin ();
}

void
raw_out (char c)
{
    cbm_k_bsout (reverse_case (last_out = c));
}

void
raw_start_error (void)
{
    cbm_k_clrch ();
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
        // Skip comment until end of line.
        if (in () == ';') {
            while (!eof () && in () >= ' ')
            while (!eof () && in () < ' ' && ch ());
            putback ();
        } else if (!isspace (c)) {
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
    outs (str);
}
