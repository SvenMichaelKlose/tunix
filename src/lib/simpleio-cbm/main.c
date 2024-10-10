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
#define CBM_EOF         0x40

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

char logical_fns[MAX_CHANNELS];
char last_errors[MAX_CHANNELS];
char eof_status[MAX_CHANNELS];
char con_flags;

void
cmd_null (void)
{
}

void
cmd_goto (void)
{
    gotoxy (cmd_params[1] - 1, cmd_params[0] - 1);
}

void
cmd_clr (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR) {
        con_flags &= ~TERM_FLAG_CURSOR;
        cursor (0);
    }
    if (c & TERM_FLAG_REVERSE) {
        con_flags &= ~TERM_FLAG_REVERSE;
        revers (0);
    }
    if (c & TERM_FLAG_DIRECT) {
        con_flags &= ~TERM_FLAG_DIRECT;
    }
}

void
cmd_set (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR) {
        con_flags |= TERM_FLAG_CURSOR;
        cursor (1);
    }
    if (c & TERM_FLAG_REVERSE) {
        con_flags |= TERM_FLAG_REVERSE;
        revers (1);
    }
    if (c & TERM_FLAG_DIRECT) {
        con_flags |= TERM_FLAG_DIRECT;
    }
}

void
cmd_get (void)
{
    putbackc (con_flags);
}

void
cmd_getx (void)
{
    putbackc (wherex () + 1);
}

void
cmd_gety (void)
{
    putbackc (wherey () + 1);
}

void
cmd_clrscr (void)
{
    cbm_k_bsout (147);
}

void
cmd_lf (void)
{
    if (con_flags & TERM_FLAG_DIRECT)
        cputc (10);
    else
        cbm_k_bsout (10);
}

void
cmd_cr (void)
{
    if (con_flags & TERM_FLAG_DIRECT)
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
    simpleio_chn_t chn;
    for (chn = FIRST_CHANNEL; chn < MAX_CHANNELS; chn++) {
        if (chn == 15)
            chn++;
        if (!logical_fns[chn]) {
            logical_fns[chn] = chn;
            eof_status[chn] = 0;
            last_errors[chn] = 0;
            return chn;
        }
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
    simpleio_chn_t chn = alloc_channel ();
    if (!chn)
        return 0;
    simpleio_init_channel (chn);

    ofs = 2;
    if (mode == 'w')
        ofs = 3;

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
    if (cbm_open (chn, 8, chn, name)) {
        logical_fns[chn] = 0;
        return 0;
    }

    // Read DOS status code.
    cbm_open (15, 8, 15, "");
    cbm_k_chkin (15);
    ctrh = cbm_k_basin ();
    ctrl = cbm_k_basin ();
    cbm_close (15);
    cbm_k_chkin (fnin);

    // Check DOS status code.
    if (ctrl != '0' || ctrh != '0') {
        last_errors[chn] = ((ctrh - '0') << 4) + (ctrl - '0');
        return 0;
    }
    return chn;
}

simpleio_chn_t
directory_open ()
{
    simpleio_chn_t chn = alloc_channel ();
    if (!chn)
        return 0;
    simpleio_init_channel (chn);
    if (!cbm_opendir (chn, 8, "$"))
        return chn;
    logical_fns[chn] = 0;
    return 0;
}

char FASTCALL
directory_read (simpleio_chn_t chn, struct cbm_dirent * dirent)
{
    return last_errors[chn] = cbm_readdir (logical_fns[chn], dirent);
}

void FASTCALL
directory_close (simpleio_chn_t chn)
{
    cbm_closedir (logical_fns[chn]);
    logical_fns[chn] = 0;
}

bool
raw_eof (void)
{
    return eof_status[fnin] > 1;
}

signed char
raw_err (void)
{
    return last_errors[fnin] & ~CBM_EOF;
}

char FASTCALL
convert_in (char c)
{
    return (fnin == STDIN) ? reverse_case (c) : c;
}

void
set_status (simpleio_chn_t chn)
{
    char s = cbm_k_readst ();
    if (s & CBM_EOF)
        eof_status[chn]++;
    last_errors[chn] = s & ~CBM_EOF;
}

char FASTCALL
raw_conin (void)
{
    char c = cgetc ();
    last_errors[fnin] = 0;
    return convert_in (c);
}

char
raw_in (void)
{
    char c = cbm_k_chrin ();
    set_status (fnin);
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
        else if (c == '|')
            c = 221;
        else
            c = reverse_case (c);
        if (con_flags & TERM_FLAG_DIRECT) {
            cputc (c);
            return;
        }
    }
    cbm_k_bsout (c);
    set_status (fnout);
}

void FASTCALL
raw_setin (simpleio_chn_t chn)
{
    cbm_k_chkin (logical_fns[chn]);
}

void FASTCALL
raw_setout (simpleio_chn_t chn)
{
    cbm_k_ckout (logical_fns[chn]);
}

void FASTCALL
raw_close (simpleio_chn_t chn)
{
    cbm_k_close (logical_fns[chn]);
    logical_fns[chn] = 0;
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
    cbm_k_clall ();
    cbm_open (STDIN, DEV_KEYBOARD, 0, NULL);
    cbm_open (STDOUT, DEV_SCREEN, 0, NULL);
    memset (logical_fns, 0, sizeof (logical_fns));
    memset (last_errors, 0, sizeof (last_errors));
    memset (eof_status, 0, sizeof (eof_status));
    logical_fns[STDIN]  = STDIN;
    logical_fns[STDOUT] = STDOUT;
    logical_fns[STDERR] = STDOUT;
    fnin  = STDIN;
    fnout = STDOUT;
    simpleio_set (&vectors);
    con_flags = TERM_FLAG_CURSOR;
}
