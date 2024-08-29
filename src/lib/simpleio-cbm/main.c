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

//#define CMD_CLRSCR  12
//#define CMD_GOTO    1
//#define CMD_CLR     2
//#define CMD_SET     3

#define TERM_FLAG_CURSOR   1
#define TERM_FLAG_REVERSE  2

void cmd_null (void);
void cmd_goto (void);
void cmd_clr (void);
void cmd_set (void);
void cmd_lf (void);
void cmd_cr (void);
void cmd_clrscr (void);

typedef void (*cmd_handler) (void);

typedef struct _con_cmd {
    char        num_params;
    cmd_handler handler;
} con_cmd;

cmd_handler cmd_current;
char cmd_params_needed;
char cmd_params[2];

con_cmd con_commands[] = {
    { 0, cmd_null },    // 0
    { 2, cmd_goto },    // 1
    { 1, cmd_clr },     // 2
    { 1, cmd_set },     // 3
    { 0, cmd_null },    // 4
    { 0, cmd_null },    // 5
    { 0, cmd_null },    // 6
    { 0, cmd_null },    // 7
    { 0, cmd_null },    // 8
    { 0, cmd_null },    // 9
    { 0, cmd_lf },      // 10
    { 0, cmd_null },    // 11
    { 0, cmd_clrscr },  // 12
    { 0, cmd_cr },      // 13
};

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
}

void
cmd_set (void)
{
    char c = cmd_params[0];
    if (c & TERM_FLAG_CURSOR)
        cursor (1);
    if (c & TERM_FLAG_REVERSE)
        revers (1);
}

void
cmd_clrscr (void)
{
    cbm_k_bsout (147);
}

void
cmd_lf (void)
{
    cbm_k_bsout (10);
}

void
cmd_cr (void)
{
    cbm_k_bsout (13);
}

void FASTCALL
raw_out (char c)
{
    // Get control code argument, call handler.
    if (cmd_params_needed) {
        cmd_params[--cmd_params_needed] = c;
        if (!cmd_params_needed) {
            cmd_current ();
            cmd_current = NULL;
        }
        return;
    }
    if (fnout == STDOUT || fnout == STDERR) {
        if (c <= 13) {
            if ((cmd_params_needed = con_commands[c].num_params))
                cmd_current = con_commands[c].handler;
            else
                con_commands[c].handler ();
            return;
        } if (c == '_')
            c = 164;
        else if (c == '\\')
            c = 205;
        else
            c = reverse_case (c);
    }
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
