#include "kernal-zp.h"

#define MAX_FILENAME    255
#define MAX_LFNS        256

#define CALL_DRIVER(name) do_nothing ()

typedef unsigned char uchar;

////////////
// GLOBAL //
////////////

#pragma data-name (push, "GLOBAL")

uchar glfn_next[MAX_LFNS];
uchar glfn_local[MAX_LFNS];
uchar free_glfn;

#pragma data-name (pop)


/////////////////
// PER-PROCESS //
/////////////////

#pragma code-name (push, "PROC")
#pragma data-name (push, "PROC")

uchar pid;

// For drivers.
uchar filename[MAX_FILENAME];
uchar glfn;
uchar reg_a;

// LFNs

uchar lfn_next[MAX_LFNS];
uchar lfn_local[MAX_LFNS];
uchar lfn_global[MAX_LFNS];
uchar first_lfn;
uchar free_lfn;

// Tempoaries.
uchar c;
uchar i;

uchar
do_nothing (void)
{
    return 0;
}

void
init_proc (void)
{
    free_lfn = 1;
    first_lfn = 0;
}

// Translate process-defined LFN to global LFN.
uchar __fastcall__
get_glfn (uchar lfn)
{
    // Look up in process pool.
    i = first_lfn;
    do {
        if (lfn_local[i] == lfn)
            return glfn = lfn_global[i];
    } while (i = lfn_next[i]);

    // Allocate global LFN.
    glfn = free_glfn;
    free_glfn = glfn_next[glfn];
    glfn_local[glfn] = lfn;

    // Allocate local LFN slot.
    i = free_lfn;
    free_lfn = lfn_next[i];
    lfn_global[i] = glfn;
    lfn_local[i] = lfn;
    lfn_next[i] = first_lfn;
    first_lfn = i;

    return glfn;
}

void
close_lfn (char i)
{
    glfn = lfn_global[i];
    glfn_next[glfn] = free_glfn;
    free_glfn = glfn;
}

// KERNAL I/O

char
open (void)
{
    // Copy file name.
    c = FNLEN;
    i = 0;
    while (c--) {
        filename[i] = FNAME[i];
        i++;
    }

    if (get_glfn (LFN))
        return CALL_DRIVER(open);
    return 0;
}

char
chkin (unsigned char lfn)
{
    if (get_glfn (lfn))
        CALL_DRIVER(chkin);
    return 0;
}

char
ckout (unsigned char lfn)
{
    if (get_glfn (lfn))
        CALL_DRIVER(chkout);
    return 0;
}

char
basin (void)
{
    CALL_DRIVER(basin);
    return 0;
}

char __fastcall__
bsout (unsigned char c)
{
    reg_a = c;
    CALL_DRIVER(bsout);
    return 0;
}

void __fastcall__
close (char lfn)
{
    if (get_glfn (lfn))
        CALL_DRIVER(close);
}

#pragma data-name (pop)
#pragma code-name (pop)
