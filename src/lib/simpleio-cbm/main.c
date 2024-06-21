#ifdef __CC65__

#ifndef __CBM__
#define __CBM__
#endif

#include <cbm.h>
#include <ingle/cc65-charmap.h>

#endif // #ifdef __CC65__

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

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

void
simpleio_open (simpleio_chn_t c, char * name, char mode)
{
    unsigned char i;
    unsigned char len = strlen (name);
    (void) mode; // TODO
    for (i = 0; i < len; i++)
        name[i] = reverse_case (name[i]);
    cbm_open (c, 8, c, name);
}

bool
raw_eof (void)
{
    return cbm_k_readst () & 0x40;
}

char
raw_err (void)
{
    return cbm_k_readst ();
}

char
raw_in (void)
{
    last_in = cbm_k_basin ();
    if (fnin == STDIN)
        last_in = reverse_case (last_in);
    return last_in;
}

void
raw_out (char c)
{
    if (fnout == STDOUT || fnout == STDERR)
        c = reverse_case (c);
    cbm_k_bsout (c);
}

void
raw_setin (simpleio_chn_t c)
{
    cbm_k_chkin (c);
}

void
raw_setout (simpleio_chn_t c)
{
    cbm_k_ckout (c);
}

void
raw_close (simpleio_chn_t c)
{
    cbm_k_clrch ();
    cbm_k_close (c);
}

simpleio vectors = {
    raw_eof,
    raw_err,
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

    // Set up standard stream LFNs.
    cbm_open (0, 0, 0, NULL);
    cbm_open (3, 3, 3, NULL);
}
