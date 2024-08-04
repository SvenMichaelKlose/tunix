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

#define  MAX_CHANNELS   15
#define  FIRST_CHANNEL  8

#define UPCASE_A  65
#define UPCASE_Z  90
#define LOCASE_A  97
#define LOCASE_Z 122
#define ISUPPER(c) (c >= UPCASE_A && c <= UPCASE_Z)
#define ISLOWER(c) (c >= LOCASE_A && c <= LOCASE_Z)
#define TOUPPER(c) (c - LOCASE_A + UPCASE_A)
#define TOLOWER(c) (c - UPCASE_A + LOCASE_A)

unsigned char i;
simpleio_chn_t next_channel;
bool channels[MAX_CHANNELS];

char
reverse_case (char c)
{
    if (ISUPPER(c))
        return TOLOWER(c);
    if (ISLOWER(c))
        return TOUPPER(c);
    return c;
}

simpleio_chn_t
alloc_channel (void)
{
    // Set empty slot to channel.
    for (i = FIRST_CHANNEL; i < MAX_CHANNELS; i++)
        if (!channels[i]) {
            channels[i] = true;
            return i;
        }
    return 0;
}

simpleio_chn_t
simpleio_open (char * name, char mode)
{
    simpleio_chn_t c = alloc_channel ();
    unsigned char len;

    // Catch error.
    if (c) {
        // Convert ASCII to whatever it is we're using for the KERNAL.
        len = strlen (name);
        for (i = 0; i < len; i++)
            name[i] = reverse_case (name[i]);

        (void) mode; // TODO
        cbm_open (c, 8, c, name);
    }

    return c;
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
    last_in = cbm_k_chrin ();
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
    channels[c] = false;
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
    bzero (channels, sizeof (channels));
    channels[STDIN] = STDIN;
    channels[STDOUT] = STDOUT;

    next_channel = FIRST_CHANNEL;
    simpleio_set (&vectors);

    // Set up standard stream LFNs.
    cbm_open (STDIN, STDIN, STDIN, NULL);
    cbm_open (STDOUT, STDOUT, STDOUT, NULL);
}
