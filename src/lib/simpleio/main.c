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
char fnin;
char fnout;
char do_putback;
char last_in;
char last_out;
#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("do_putback")
#pragma zpsym ("last_in")
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
raw_err (void)
{
    return raw_eof ();
}

char
raw_in (void)
{
    return cbm_k_basin ();
}

void
raw_out (char c)
{
    if (fnout == STDOUT)
        c = reverse_case (c);
    cbm_k_bsout (c);
}

void
reset (void)
{
    cbm_k_clrch ();
    if (fnin != STDIN)
        cbm_k_chkin (fnin);
    if (fnout != STDOUT)
        cbm_k_ckout (fnout);
}

void
raw_setin (char c)
{
    if (c == STDIN)
        reset ();
    else 
        cbm_k_chkin (fnin);
}

void
raw_setout (char c)
{
    if (c == STDOUT)
        reset ();
    else 
        cbm_k_ckout (fnout);
}

void
setin (char c)
{
    if (fnin != c) {
        fnin = c;
        raw_setin (c);
    }
}

void
setout (char c)
{
    if (fnout != c) {
        fnout = c;
        raw_setout (c);
    }
}

char
eof ()
{
    return raw_eof ();
}

char
err ()
{
    return raw_err ();
}

char
in ()
{
    if (do_putback)
        do_putback = false;
    else
        last_in = raw_in ();
    return last_in;
}

char
ch ()
{
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
            while (!eof () && in () >= ' ')
            while (!eof () && in () < ' ' && ch ());
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
    raw_out (c);
}

void
outnu (unsigned n)
{
    unsigned a;
    if (n > 9) {
        a = n / 10;
        n -= 10 * a;
        outnu (a);
    }
    out ('0' + n);
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
terpri (void)
{
    outs ("\n\r");
}

void
fresh_line (void)
{
    if (last_in >= ' ')
#if TARGET == c128
        out ('\n');
#else
        outs ("\n\r");
#endif
}

void
errouts (char * str)
{
    setout (STDOUT);
    outs (str);
}
