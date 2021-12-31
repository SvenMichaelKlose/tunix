#pragma code-name ("ULTIFS")

#include <lib/ingle/cc65-charmap.h>

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <conio.h>
#include <dirent.h>

#include <lib/ultimem/ultimem.h>
#include <lib/posix/string.h>

#include "ultifs.h"

#define FALSE   0
#define TRUE    -1

// Callee registers
#define accu        (*(char*) 0x100)
#define xreg        (*(char*) 0x101)
#define yreg        (*(char*) 0x102)
#define flags       (*(char*) 0x103)

// CPU flags
#define FLAG_C          1
#define FLAG_Z          2
#define FLAG_I          4
#define FLAG_D          8
#define FLAG_B          16
#define FLAG_UNUSED     32
#define FLAG_V          64
#define FLAG_N          128

/* KERNAL zero page data */
#define FNLEN   (*(char*)  0xb7)    // File name length
#define LFN     (*(char*)  0xb8)    // Logical file number
#define SA      (*(char*)  0xb9)    // Secondary address
#define FA      (*(char*)  0xba)    // Device number
#define FNAME   (*(char**) 0xbb)    // File name pointer
#define STATUS  (*(char*)  0x90)    // Serial line status byte

/* Serial line error codes */
#define STATUS_NO_DEVICE        0x80
#define STATUS_END_OF_FILE      0x40
#define STATUS_CHECKSUM_ERROR   0x20
#define STATUS_READ_ERROR       0x10
#define STATUS_LONG             0x08
#define STATUS_SHORT            0x04
#define STATUS_TIMEOUT_READ     0x02
#define STATUS_TIMEOUT_WRITE    0x01

/* KERNAL error codes */
#define OSERR_FILE_NOT_OPEN         3
#define OSERR_DEVICE_NOT_PRESENT    5
#define OSERR_FILE_NOT_IN           6
#define OSERR_FILE_NOT_OUT          7

/* Device error codes (sent as message) */
#define ERR_BYTE_DECODING       24
#define ERR_WRITE               25
#define ERR_WRITE_PROTECT_ON    26
#define ERR_SYNTAX              30
#define ERR_INVALID_COMMAND     31
#define ERR_LONG_LINE           32
#define ERR_INVALID_FILE_NAME   33
#define ERR_NO_FILE_GIVEN       34
#define ERR_FILE_TOO_LARGE      52
#define ERR_FILE_OPEN_FOR_WRITE 60
#define ERR_FILE_NOT_OPEN       61
#define ERR_FILE_NOT_FOUND      62
#define ERR_FILE_EXISTS         63
#define ERR_FILE_TYPE_MISMATCH  64
#define ERR_NO_CHANNEL          70
#define ERR_DISK_FULL           72


typedef struct _channel {
    char *      name;
    bfile *     file;
    char        sa;

    char *      buf;
    char *      bufwptr;
    char *      bufrptr;
} channel;

#define MAX_USER_LFN  31    // TODO: Find out the official maximum.
#define LOADSAVE_LFN  (MAX_USER_LFN + 1)

// Channels keyed by logical file naumber.
channel * channels[LOADSAVE_LFN + 1];

channel cmd_channel = {
    NULL, NULL, 0, NULL, NULL, NULL
};


extern char ultifs_kopen  (void);
extern void ultifs_kclose (void);
extern void ultifs_kclall (void);
extern void ultifs_kload  (void);
extern void ultifs_ksave  (void);


void
init_kernal_emulation ()
{
    bzero (channels, sizeof (channels));

    channels[12] = &cmd_channel;

    if (ultimem_unhide () != ULTIMEM_UNHIDE_ID_ULTIMEM) {
        printf ("No UltiMem found - exiting.\n");
        exit (0);
    }

    ultifs_mount ();
}

// TODO: This'll map the calling process' memory to BLK5.
void
copy_from_process (char * to, char * from, char len)
{
    memcpy (to, from, len);
/*
    char blk5 = *ULTIMEM_BLK5;
    char * ptr;

    while (len--) {
        ptr = ultimem_map_ptr (from++, (void *) 0xa000, (int *) 0x104, ULTIMEM_BLK5);
        *to = *ptr;
    }

    *ULTIMEM_BLK5 = blk5;
*/
}

void
set_oserr (char code)
{
    accu = code;
    flags = code ? FLAG_C : 0;
}

void
set_error (char code)
{
    channel * ch = channels[15];

    if (ch->buf) {
        free (ch->buf);
        ch->buf = ch->bufwptr = NULL;
    }

    if (!code)
        return;

    ch->buf = ch->bufwptr = malloc (64);
    sprintf (ch->buf, "%d, ERROR, 0, 0", code);
}

void
add_to_buf (channel * ch, void * ptr, size_t len)
{
    if (!ch->buf)
        ch->buf = ch->bufrptr = ch->bufwptr = malloc (2048);

    memcpy (ch->bufwptr, ptr, len);
    ch->bufwptr += len;
}

char
read_from_buf (channel * ch)
{
    char c = *ch->bufrptr++;

    if (ch->bufrptr == ch->bufwptr) {
        free (ch->buf);
        ch->buf = NULL;
    }

    return c;
}

void
open_command (char * name)
{
    channel * ch = channels[15];
    char * msg = "fooooo!!";

    add_to_buf (ch, msg, strlen (msg));

    STATUS = 0;
}

void
make_directory_list (channel * ch)
{
    unsigned line_addr = 0x1201;
    struct cbm_dirent * dirent = malloc (sizeof (struct cbm_dirent));
    char * line = malloc (256);
    unsigned char i;
    unsigned char l;
    char c;
    char * p;
    size_t len;

    // Emit start address.
    line[0] = line_addr & 255;
    line[1] = line_addr >> 8;
    add_to_buf (ch, line, 2);

    ultifs_opendir ();

    // Emit directory entries as BASIC lines.
    while (!ultifs_readdir (dirent)) {
        p = line + 2;

        *p++ = dirent->size & 255;
        *p++ = dirent->size >> 8;
        *p++ = '"';
        l = strnlen (dirent->name, 17);
        for (i = 0; i < l; i++) {
            c = toupper (dirent->name[i]);
            *p++ = c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c;
        }
        *p++ = '"';
        *p++ = ' ';
        *p++ = 'P';
        *p++ = 'R';
        *p++ = 'G';
        *p++ = 0;

        len = p - line;
        line_addr += len;
        line[0] = line_addr & 255;
        line[1] = line_addr >> 8;

        add_to_buf (ch, line, len);
    }

    // Emit EOT (high byte of next line set to 0).
    line[0] = line[1] = 0;
    add_to_buf (ch, line, 2);

    ultifs_closedir ();

    free (dirent);
    free (line);

    set_oserr (0);
}

void
free_channel ()
{
    channel * ch = channels[LFN];

    free (ch->name);
    free (ch->buf);
    free (ch);

    channels[LFN] = NULL;
}

char
ultifs_kopen ()
{
    char *      name = NULL;
    bfile *     found_file;
    channel *   ch;

    if (SA != 15 && channels[LFN]) {
        set_error (ERR_NO_CHANNEL);
        return FALSE;
    }

    if (FNLEN) {
        name = malloc (FNLEN + 1);
        if (!name) {
            set_error (ERR_BYTE_DECODING);
            return FALSE;
        }
        copy_from_process (name, FNAME, FNLEN);
        name[FNLEN] = 0;
    }

    ch = malloc (sizeof (channel));
    ch->buf = NULL;
    channels[LFN] = ch;

    if (SA == 15) {
        open_command (name);
        return TRUE;
    }

    if (FNLEN == 1 && *name == '$') {
        make_directory_list (ch);
        return TRUE;
    }

    found_file = ultifs_open (ultifs_pwd, name, 0);
    if (!found_file) {
        set_error (ERR_FILE_NOT_FOUND);
        goto error;
    }

    ch->file = found_file;

    set_error (0);
    return TRUE;

error:
    free_channel ();

    return FALSE;
}

void
ultifs_kclose ()
{
    channel * ch = channels[LFN];

    if (!ch) {
        set_error (ERR_FILE_NOT_OPEN);
        return;
    }

    if (SA == 15) {
        ultifs_kclall ();
        return;
    }

    bfile_close (ch->file);
    free_channel ();
}

void
ultifs_kchkin ()
{
    set_oserr (channels[LFN] ? 0 : OSERR_FILE_NOT_OPEN);
}

void
ultifs_kchkout ()
{
    if (!channels[LFN]) {
        set_oserr (OSERR_FILE_NOT_OPEN);
        return;
    }

    if (SA != 15) {
        set_oserr (OSERR_FILE_NOT_OUT);
        return;
    }

    // TODO: Complete Flash writes.
    set_oserr (0);
}

void
ultifs_kclrcn ()
{
}

char
ultifs_kbasin ()
{
    channel * ch = channels[LFN];

    if (!ch) {
        set_oserr (OSERR_FILE_NOT_OPEN);
        return 0;
    }

    STATUS = 0;

    if (ch->buf) {
        accu = read_from_buf (ch);
        if (!ch->buf)
            goto end_of_file;
        return accu;
    }

    //if (SA == 15)
        //return;

    // TODO: bfile reads.

end_of_file:
    STATUS = STATUS_END_OF_FILE;

    return accu;
}

void
ultifs_kbsout ()
{
    // TODO: Implement writes.
    STATUS = STATUS_TIMEOUT_WRITE;
}

void
ultifs_kclall ()
{
}

void
ultifs_kload ()
{
    char do_verify = accu;
    char * addr;
    unsigned addr_l;
    unsigned addr_h;
    char status;

    LFN = LOADSAVE_LFN;

    if (!ultifs_kopen ())
        return;

    // Read destination address.
    addr_l = ultifs_kbasin ();
    addr_h = ultifs_kbasin ();

    // Override by YX pair when SA == 0.
    if (!SA) {
        addr_l = xreg;
        addr_h = yreg;
    } else if (SA > 2) {
        // TODO: Issue some error?
    }
    addr = (char *) (addr_h << 8 | addr_l);

    while (1) {
        *addr++ = ultifs_kbasin ();

        if (STATUS & STATUS_END_OF_FILE)
            break;
        if (STATUS) {
            status = STATUS;
            ultifs_kclose ();
            STATUS = status;
            flags = FLAG_C;         // TODO: Does this really happen?
            return;
        }
    }

    addr--;
    xreg = (unsigned) addr & 255;
    yreg = (unsigned) addr >> 8;
    flags = 0;

    ultifs_kclose ();
}

void
ultifs_ksave ()
{
    char * ptr = *(char **) accu;
    char * end = (char*) (yreg << 8 + xreg);
    char status;

    LFN = LOADSAVE_LFN;

    if (!ultifs_kopen ())
        return;

    while (1) {
        accu = *ptr++;
        ultifs_kbsout ();

        if (STATUS) {
            status = STATUS;
            ultifs_kclose ();
            STATUS = status;
            flags = FLAG_C;     // TODO: Does this really happen?
            return;
        }

        if (end == ptr)
            break;
    }

    ultifs_kclose ();
}
