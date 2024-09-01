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
#include <conio.h>

#include <simpleio/libsimpleio.h>
#include <simpleio/control.h>

#define FIRST_CHANNEL  8

#define DEV_KEYBOARD    0
#define DEV_SCREEN      3

// cc65 charmap-independent values
#define UPCASE_A  65
#define UPCASE_Z  90
#define LOCASE_A  97
#define LOCASE_Z 122
#define ISUPPER(c) (c >= UPCASE_A && c <= UPCASE_Z)
#define ISLOWER(c) (c >= LOCASE_A && c <= LOCASE_Z)
#define TOUPPER(c) (c - LOCASE_A + UPCASE_A)
#define TOLOWER(c) (c - UPCASE_A + LOCASE_A)

#define CHN_USED    1

char            logical_fns[MAX_CHANNELS];
simpleio_chn_t  chn;
signed char     last_status[MAX_CHANNELS];

void
cmd_null (void)
{
}

void
cmd_goto (void)
{
    gotoxy (cmd_params[1], cmd_params[0]);
}

void
cmd_clr (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR)
        cursor (0);
    if (c & TERM_FLAG_REVERSE)
        revers (0);
    if (c & TERM_FLAG_DIRECT)
        term_direct_mode = false;
}

void
cmd_set (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR)
        cursor (1);
    if (c & TERM_FLAG_REVERSE)
        revers (1);
    if (c & TERM_FLAG_DIRECT)
        term_direct_mode = true;
}

void
cmd_clrscr (void)
{
    cbm_k_bsout (147);
}

void
cmd_lf (void)
{
    if (term_direct_mode)
        cputc (10);
    else
        cbm_k_bsout (10);
}

void
cmd_cr (void)
{
    if (term_direct_mode)
        cputc (13);
    else
        cbm_k_bsout (13);
}


char FASTCALL
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
    for (chn = FIRST_CHANNEL; chn < MAX_CHANNELS; chn++)
        if (!logical_fns[chn]) {
            logical_fns[chn] = chn;
            return chn;
        }
    return 0;
}

signed char   ofs;
char          ctrl;
char          ctrh;
unsigned char i;

// XXX: ld65 complains about this being a duplicate if named 'len',
// but does not tell in which places it occurs.
unsigned char silen;

// DANGEROUS but we need the heap:
// 'name' is modified and extended on writes.
simpleio_chn_t FASTCALL
simpleio_open (char * name, char mode)
{
    chn = alloc_channel ();
    last_status[chn] = 0;
    if (!chn)
        goto error;
    simpleio_init_channel (chn);

    ofs = 2;
    if (mode == 'w')
        ofs = 3;
#ifndef NDEBUG
    // Must be checked by caller already.
    else if (mode != 'r')
        goto error;
#endif

    silen = strlen (name);

    // Move name and reverse case.
    for (i = silen; i != 255; i--)
        name[i + ofs] = reverse_case (name[i]);

    if (mode == 'w') {
        name[0] = '@';
        name[1] = '0';
        name[2] = ':';
        strcpy (name + silen + ofs, ",S,W");
    } else {
        name[0] = '0';
        name[1] = ':';
        strcpy (name + silen + ofs, ",S,R");
    }

    // Open file.
    if (cbm_open (chn, 8, chn, name))
        goto error;

    // Read and check DOS status code.
    cbm_open (15, 8, 15, "");
    cbm_k_chkin (15);
    ctrh = cbm_k_basin ();
    ctrl = cbm_k_basin ();
    cbm_close (15);
    cbm_k_chkin (fnin);
    if (ctrl != '0' || ctrh != '0') {
        last_status[chn] = ((ctrh - '0') << 4) + (ctrl - '0');
        return 0;
    }

    return chn;

error:
    last_status[chn] = -1;
    return 0;
}

simpleio_chn_t
directory_open ()
{
    chn = alloc_channel ();
    last_status[chn] = 0;
    if (!chn)
        goto error;
    simpleio_init_channel (chn);
    if (cbm_opendir (chn, 8, "$"))
        goto error;
    return chn;
error:
    last_status[chn] = -1;
    return 0;
}

char FASTCALL
directory_read (simpleio_chn_t chn, struct cbm_dirent * dirent)
{
    return last_status[chn] = cbm_readdir (logical_fns[chn], dirent);
}

void FASTCALL
directory_close (simpleio_chn_t chn)
{
    cbm_closedir (logical_fns[chn]);
    logical_fns[chn] = 0;
    last_status[chn] = 0;
}

bool
raw_eof (void)
{
    return last_status[chn];
}

signed char
raw_err (void)
{
    return last_status[chn] & ~0x40;
}

char FASTCALL
convert_in (char c)
{
    return (fnin == STDIN) ? reverse_case (c) : c;
}

void
set_status (void)
{
    last_status[chn] = cbm_k_readst ();
}

char FASTCALL
raw_conin (void)
{
    char c = cgetc ();
    set_status ();
    return convert_in (c);
}

char
raw_in (void)
{
    char c = cbm_k_chrin ();
    set_status ();
    return convert_in (c);
}

void FASTCALL
raw_out (char c)
{
    if (simpleio_control (c))
        return;
    if (fnout == STDOUT || fnout == STDERR) {
        if (c == '_')
            c = 164;
        else if (c == '\\')
            c = 205;
        else
            c = reverse_case (c);
    }
    if (term_direct_mode)
        cputc (c);
    else
        cbm_k_bsout (c);
    set_status ();
}

void FASTCALL
raw_setin (simpleio_chn_t chn)
{
    cbm_k_chkin (logical_fns[chn]);
    set_status ();
}

void FASTCALL
raw_setout (simpleio_chn_t chn)
{
    cbm_k_ckout (logical_fns[chn]);
    set_status ();
}

void FASTCALL
raw_close (simpleio_chn_t chn)
{
    cbm_k_close (logical_fns[chn]);
    logical_fns[chn] = 0;
    last_status[chn] = 0;
}

simpleio vectors = {
    raw_eof,
    raw_err,
    raw_conin,
    raw_in,
    raw_out,
    raw_setin,
    raw_setout,
    raw_close
};

void
simpleio_init ()
{
    cbm_open (STDIN, DEV_KEYBOARD, 0, NULL);
    cbm_open (STDOUT, DEV_SCREEN, 0, NULL);
    memset (logical_fns, 0, sizeof (logical_fns));
    logical_fns[STDIN]  = STDIN;
    logical_fns[STDOUT] = STDOUT;
    logical_fns[STDERR] = STDOUT;
    simpleio_set (&vectors);
}
