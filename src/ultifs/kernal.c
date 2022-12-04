#pragma code-name ("ULTIFS")

#include <lib/ingle/cc65-charmap.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <cbm.h>

#include <lib/ultimem/ultimem.h>
#include <lib/posix/string.h>

#include "log.h"
#include "ultifs.h"

typedef unsigned char uchar;

extern uchar accu;
extern uchar xreg;
extern uchar yreg;
extern uchar flags;
extern uchar cfg;
extern uchar blk1;
extern uchar blk2;
extern uchar blk3;
extern uchar blk5;

#define proc_ustatus  cfg
#define proc_blk1     blk1
#define proc_blk2     blk2
#define proc_blk3     blk3
#define proc_blk5     blk5

// CPU flags
#define FLAG_C       1
#define FLAG_Z       2
#define FLAG_I       4
#define FLAG_D       8
#define FLAG_B       16
#define FLAG_UNUSED  32
#define FLAG_V       64
#define FLAG_N       12

/* KERNAL zero page data */
#define FNLEN   (*(uchar *) 0xb7)    // File name length
#define LFN     (*(uchar *) 0xb8)    // Logical file number
#define SA      (*(uchar *) 0xb9)    // Secondary address
#define FA      (*(uchar *) 0xba)    // Device number
#define FNAME   (*(char **) 0xbb)    // File name pointer
#define STATUS  (*(uchar *) 0x90)    // Serial line status byte
#define LDTND   (*(uchar *) 0x98)    // Number of open files
#define DFLTN   (*(uchar *) 0x99)    // Logical input file number
#define DFLTO   (*(uchar *) 0x9a)    // Logical output file number

#define SAP     (*(char **) 0xac)    // Load/save current address
#define SAL     (*(uchar *) 0xac)
#define SAH     (*(uchar *) 0xad)
#define EAP     (*(char **) 0xae)    // Load/save end address
#define EAL     (*(uchar *) 0xae)
#define EAH     (*(uchar *) 0xaf)

/*
#define LAT     (*(uchar *) 0x0259)  // File number table
#define FAT     (*(uchar *) 0x0263)  // Device number table
#define SAT     (*(uchar *) 0x026d)  // Secondary address table
*/

/* STATUS: Serial line error codes */
#define STATUS_NO_DEVICE       0x80
#define STATUS_END_OF_FILE     0x40
#define STATUS_CHECKSUM_ERROR  0x20
#define STATUS_READ_ERROR      0x10
#define STATUS_LONG            0x08
#define STATUS_SHORT           0x04
#define STATUS_TIMEOUT_READ    0x02
#define STATUS_TIMEOUT_WRITE   0x01

/* KERNAL error codes */
#define OSERR_TOO_MANY_FILES         1
#define OSERR_FILE_ALREADY_OPEN      2
#define OSERR_FILE_NOT_OPEN          3
#define OSERR_FILE_NOT_FOUND         4
#define OSERR_DEVICE_NOT_PRESENT     5
#define OSERR_FILE_NOT_IN            6
#define OSERR_FILE_NOT_OUT           7
#define OSERR_MISSING_FILE_NAME      8
#define OSERR_ILLEGAL_DEVICE_NUMBER  9

/* Device error codes (read from command channel #15) */
#define ERR_BYTE_DECODING        24
#define ERR_WRITE                25
#define ERR_WRITE_PROTECT_ON     26
#define ERR_SYNTAX               30
#define ERR_INVALID_COMMAND      31
#define ERR_LONG_LINE            32
#define ERR_INVALID_FILE_NAME    33
#define ERR_NO_FILE_GIVEN        34
#define ERR_FILE_TOO_LARGE       52
#define ERR_FILE_OPEN_FOR_WRITE  60
#define ERR_FILE_NOT_OPEN        61
#define ERR_FILE_NOT_FOUND       62
#define ERR_FILE_EXISTS          63
#define ERR_FILE_TYPE_MISMATCH   64
#define ERR_NO_CHANNEL           70
#define ERR_DISK_FULL            72

extern uchar global_lfns[256];

typedef struct _channel {
    char *   name;          // File name
    bfile *  file;          // UltiFS file
    char     sa;            // Secondary address
    char     is_buffered;   // As of now: reads from buffer with
                            // generated content (e.g. directory listing).

    char *   buf;
    char *   bufwptr;       // Fill pointer
    char *   bufrptr;       // Read pointer
} channel;

#define NUM_LFN  255    // No limit. LFNs 128>= should add extra line feeds. (pixel)

channel * channels[256];
char * response;                        // Error code of last operation.

void
init_kernal_emulation ()
{
    if (ultimem_unhide () != ULTIMEM_UNHIDE_ID_ULTIMEM) {
        printf ("No UltiMem found - exiting.\n");
        exit (0);
    }

    bzero (channels, sizeof (channels));
    response = NULL;

    ultifs_mount ();
}


//
// Read/write client memory
//

char
peek_from_process (char * p)
{
    uchar ph = (unsigned) p >> 8;

    if (ph < 0x20 || ph > 0x7f)
        return *p;

    *ULTIMEM_CONFIG2 |= 0xc0;

    if (ph < 0x40) {
        *ULTIMEM_BLK5 = proc_blk1;
        return *(p - (char *) 0x2000 + (char *) 0xa000);
    }
    if (ph < 0x60) {
        *ULTIMEM_BLK5 = proc_blk2;
        return *(p - (char *) 0x4000 + (char *) 0xa000);
    }
    if (ph < 0x80) {
        *ULTIMEM_BLK5 = proc_blk3;
        return *(p - (char *) 0x6000 + (char *) 0xa000);
    }

    return *p;
}

char __fastcall__
poke_to_process (char * ptr, char v)
{
    register char * p = ptr;
    register uchar ph = (unsigned) p >> 8;

    if (ph < 0x20 || ph > 0x7f)
        return *p = v;

    *ULTIMEM_CONFIG2 |= 0xc0;

    if (ph < 0x40)
        *ULTIMEM_BLK5 = proc_blk1;
    else if (ph < 0x60)
        *ULTIMEM_BLK5 = proc_blk2;
    else if (ph < 0x80)
        *ULTIMEM_BLK5 = proc_blk3;

    p &= 0x1fff;
    p += 0xa000;
    return *p = v;
}

char
downcase (char c)
{
    if (c >= 'A' && c <= 'Z')
        return c - 'A' + 'a';
    return c;
}

void
copy_from_process (char * to, char * from, char len)
{
    while (len--)
        *to++ = downcase (peek_from_process (from++));
}

char prefix;
char filename[32];
char param1;
char param2;

bool
parse_name (char * i)
{
    char * o = filename;
    char c;

    filename[0] = param1 = param2 = 0;
    if (!FNLEN)
        return false;
    if (FNLEN > 2 && i[1] == ':') {
        prefix = i[0];
        i += 2;
    }

    while (c = *i++) {
        if (!c || c == ',')
            break;
        *o++ = c;
    }

    if (!c)
        return true;
    param1 = *i++;
    c = *i++;
    if (!c)
        return true;
    if (c != ',')
        return false;
    param2 = *i++;
    return !*i++;
}

//
// Channel buffers
//

void
clear_buf (channel * ch)
{
    free (ch->buf);
    ch->buf = NULL;
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

    if (ch->bufrptr == ch->bufwptr)
        clear_buf (ch);

    return c;
}


//
// Control channel #15
//

void
respond (char code, char * message)
{
    free (response);
    response = malloc (64);
    sprintf (response, "%#02D, %S, 00, 00\r", code, message);
    log_message ("R: '%S'.", response);
}

void
respond_ok ()
{
    respond (0, "ok");
}

void
respond_err_syntax ()
{
    respond (ERR_SYNTAX, "syntax error");
}

void
cmd_initialize ()
{
    respond_ok ();
}

void
open_command (char * name)
{
    channel *  ch = channels[LFN];

    switch (name[0]) {
        case 'i':
            cmd_initialize ();
            return;
    }

    respond_err_syntax ();
    STATUS = 0;
}

void
make_directory_list (channel * ch)
{
    struct cbm_dirent * dirent = malloc (sizeof (struct cbm_dirent));
    char *    line = malloc (256);
    unsigned  line_addr = 0x1201;
    uchar     i;
    uchar     l;
    char      c;
    char *    p;
    size_t    len;
    char *    type;

    // Emit start address.
    line[0] = line_addr & 255;
    line[1] = line_addr >> 8;
    add_to_buf (ch, line, 2);

    ultifs_opendir ();

    // Emit directory entries as BASIC lines.
    while (!ultifs_readdir (dirent)) {
        // Skip pointer to next line. (Updated later.)
        p = line + 2;

        // Write file size to line number.
        *p++ = dirent->size & 255;
        *p++ = dirent->size >> 8;

        // Write file name in quotes.
        *p++ = '"';
        l = strnlen (dirent->name, 17);
        for (i = 0; i < l; i++) {
            c = toupper (dirent->name[i]);
            *p++ = c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c;
        }
        *p++ = '"';

        *p++ = ' ';

        // Write file type.
        switch (dirent->type) {
            case CBM_T_DEL:
                type = "DEL"; break;
            case CBM_T_SEQ:
                type = "SEQ"; break;
            case CBM_T_PRG:
                type = "PRG"; break;
            case CBM_T_USR:
                type = "USR"; break;
            case CBM_T_REL:
                type = "REL"; break;
            case CBM_T_CBM:
                type = "CBM"; break;
            case CBM_T_DIR:
                type = "DIR"; break;
            case CBM_T_LNK:
                type = "LNK"; break;
            case CBM_T_VRP:
                type = "VRP"; break;
            case CBM_T_HEADER:
                type = "HDR"; break;
            case CBM_T_OTHER:
            default:
                type = "???"; break;
        }
        *p++ = *type++;
        *p++ = *type++;
        *p++ = *type;

        *p++ = 0;

        // Update pointer to next line.
        len = p - line;
        line_addr += len;
        line[0] = line_addr & 255;
        line[1] = line_addr >> 8;

        //  Output line.
        add_to_buf (ch, line, len);
    }

    // Emit EOT (high byte of pointer to next line is 0).
    line[0] = line[1] = 0;
    add_to_buf (ch, line, 2);

    free (dirent);
    free (line);

    ultifs_closedir ();
    flags &= ~FLAG_C;
}


//
// KERNAL emulation
//

void
free_channel (char lfn)
{
    channel * ch = channels[lfn];
    log_message ("free channel %D.", lfn);

    clear_buf (ch);
    free (ch->name);
    free (ch);

    channels[lfn] = NULL;
}

char
ultifs_kopen ()
{
    char *     name = NULL;
    bfile *    found_file;
    channel *  ch;

    if (!LFN) {
        accu = OSERR_FILE_NOT_IN;
        goto error;
    }
    if (channels[LFN]) {
        accu = OSERR_FILE_ALREADY_OPEN;
        goto error;
    }

    STATUS = 0;
    flags &= ~FLAG_C;

    if (FNLEN) {
        name = malloc (FNLEN + 1);
        copy_from_process (name, FNAME, FNLEN);
        name[FNLEN] = 0;
        parse_name (name);
        strcpy (name, filename);
    }

    ch = malloc (sizeof (channel));
    ch->sa = SA;
    ch->is_buffered = false;
    ch->buf = NULL;
    ch->file = NULL;
    ch->name = name;
    channels[LFN] = ch;
    log_message ("open %D, %D, %D, '%S'.", LFN, FA, SA, name);

    if (SA == 15) {
        ch->is_buffered = true;
        open_command (name);
        goto success;
    }

    if (FNLEN == 1 && *name == '$') {
        ch->is_buffered = true;
        make_directory_list (ch);
        goto success;
    }

    if (!param1 || ((param1 == 's' || param1 == 'p') && (!param2 || param2 == 'r'))) {
        found_file = ultifs_open (ultifs_pwd, name, 0);
        if (!found_file) {
            respond (ERR_FILE_NOT_FOUND, "file not found");
            free_channel (LFN);
            return false;
        }

        ch->file = found_file;
        respond_ok ();
        goto success;
    }

    if (param2 == 'w') {
        if (param1 != 's' && param2 != 'p') {
            respond_err_syntax ();
            return false;
        }
        found_file = ultifs_open (ultifs_pwd, name, 0);
        if (found_file) {
            bfile_close (found_file);
            respond (ERR_FILE_EXISTS, "file exists");
            return false;
        }

        ch->file = ultifs_create (ultifs_pwd, name);
        respond_ok ();
        goto success;
    }

    if (param2 == 'a') {
        respond (ERR_WRITE_PROTECT_ON, "write protect on");
        free_channel (LFN);
        return false;
    }

    respond (ERR_INVALID_FILE_NAME, "invalid file name");
error:
    flags |= FLAG_C;
    return false;

success:
    log_message ("<open %D.", LFN);
    global_lfns[LFN] = LFN;
    return true;
}

void
ultifs_kclrcn ()
{
    log_message ("clrcn");
}

void
ultifs_kchkin ()
{
    log_message ("chkin '%D'.", xreg);
    if (!channels[xreg]) {
        accu = OSERR_FILE_NOT_OPEN;
        goto error;
    }
    if (channels[xreg]->file->mode != ULTIFS_MODE_READ) {
        accu = OSERR_FILE_NOT_IN;
        goto error;
    }
    DFLTN = xreg;
    accu = 0;
    flags &= ~FLAG_C;
    return;

error:
    flags |= FLAG_C;
}

void
ultifs_kckout ()
{
    log_message ("ckout '%D'.", xreg);
    if (!channels[xreg]) {
        flags |= FLAG_C;
        accu = OSERR_FILE_NOT_OPEN;
        return;
    }
    if (channels[xreg]->file->mode != ULTIFS_MODE_WRITE) {
        flags |= FLAG_C;
        accu = OSERR_FILE_NOT_OUT;
        return;
    }
    DFLTO = xreg;
    flags &= ~FLAG_C;
    accu = 0;
}

char
ultifs_kbasin ()
{
    register channel *  ch = channels[DFLTN];
    register bfile *    file;
    log_message ("basin '%D'. ", DFLTN);

    if (!ch) {
        flags |= FLAG_C;
        return accu = 0;
    }

    STATUS = 0;
    flags &= ~FLAG_C;

    if (ch->is_buffered) {
        if (!ch->buf) {
            if (ch->sa != 15 || !response)
                goto end_of_file;
            add_to_buf (ch, response, strlen (response));
            free (response);
            response = NULL;
        }
        return accu = read_from_buf (ch);
    }

    file = ch->file;
    if (file->mode != ULTIFS_MODE_READ) {
        flags |= FLAG_C;
        return OSERR_FILE_NOT_IN;
    }
    accu = bfile_read (file);
    if (ultifs_error == ULTIFS_ERR_OK)
        return accu;

end_of_file:
    STATUS = STATUS_END_OF_FILE;
    return 0;
}

void
ultifs_kbsout ()
{
    register channel *  ch = channels[DFLTO];
    log_message ("bsout '%D'. ", DFLTO);

    if (!ch) {
        flags |= FLAG_C;
        STATUS |= STATUS_TIMEOUT_WRITE;
        return;
    }
    if (ch->file->mode != ULTIFS_MODE_WRITE) {
        flags |= FLAG_C;
        STATUS |= STATUS_TIMEOUT_WRITE;
        return;
    }

    bfile_write (ch->file, accu);
    STATUS = 0;
    flags &= ~FLAG_C;
}

void
ultifs_kclose ()
{
    channel * ch = channels[accu];
    log_message ("close '%D'. ", accu);
    if (ch->file)
        bfile_close (ch->file);
    free_channel (accu);
    global_lfns[accu] = 0xff;
}

void
ultifs_kclall ()
{
    uchar  i;
    log_message ("clall");

    do {
        if (channels[i]) {
            accu = i;
            ultifs_kclose ();
        }
    } while (++i);
}

void
ultifs_kload ()
{
    char   do_verify = accu;
    char   old_LFN = LFN;
    LFN = NUM_LFN - 1;
    if (!ultifs_kopen ())
        goto end;

    flags &= ~FLAG_C;

    // Prepare reads.
    DFLTN = LFN;

    // Read destination address.
    EAL = ultifs_kbasin ();
    EAH = ultifs_kbasin ();

    // When SA=0 override with address to YX.
    if (!SA) {
        EAL = xreg;
        EAH = yreg;
    }

    // Read all bytes.
    while (!STATUS) {
        ultifs_kbasin ();
        if (STATUS & STATUS_END_OF_FILE)
            break;
        poke_to_process (EAP++, accu);
    }

    accu = LFN;
    ultifs_kclose ();

    // Return next free address.
    xreg = EAL;
    yreg = EAH;

end:
    LFN = old_LFN;
}

void
ultifs_ksave ()
{
    char   old_LFN = LFN;
    LFN = NUM_LFN - 1;
    if (!ultifs_kopen ())
        goto end;

    SAP = *(char **) accu;
    EAP = (void *) (yreg << 8 + xreg);
    flags &= ~FLAG_C;

    // Prepare writes.
    DFLTO = LFN;

    // Save address.
    accu = SAL;
    ultifs_kbsout ();
    accu = SAH;
    ultifs_kbsout ();

    // Write all bytes.
    while (!STATUS) {
        accu = peek_from_process (SAP++);
        ultifs_kbsout ();
        if (EAP == SAP)
            break;
    }

    accu = LFN;
    ultifs_kclose ();

end:
    LFN = old_LFN;
}
