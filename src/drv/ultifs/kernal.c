#pragma code-name ("ULTIFS")

#include <ingle/cc65-charmap.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <cbm.h>

#include <log/liblog.h>
#include <ultimem/ultimem.h>
#include <posix/string.h>

#include "ultifs.h"
#include "kernal.h"
#include "ipc.h"

#define SEC()   (flags |= FLAG_C)
#define CLC()   (flags &= ~FLAG_C)

channel * channels[256];

char * response;                    // Error code of last operation.

char    fullname[MAX_FNLEN];        // Complete command string.

// Parsed: 'fullname':
bool    has_prefix;
bool    has_params;
uchar   num_params;
char    prefix[MAX_FNLEN];       // Everything up to the colon (':').
char    pathname[MAX_FNLEN];     // From colon till params (starting with ',').
char    params[MAX_FNLEN];       // All params queued.
char *  param_list[MAX_FPARAMS]; // Pointers to each param in 'params'.
char    filename[MAX_FNLEN];     // Last path component(s) that does not exist.

upos directory;


void
init_kernal_emulation ()
{
    if (ultimem_unhide () != ULTIMEM_UNHIDE_ID_ULTIMEM) {
        printf ("No UltiMem found - exiting.\n");
        exit (0);
    }

    memset (channels, 0, sizeof (channels));
    response = NULL;

    ultifs_mount ();
}

char __fastcall__
downcase (char c)
{
    if (c >= 'A' && c <= 'Z')
        return c - 'A' + 'a';
    return c;
}

void __fastcall__
copy_from_process (char * to, char * from, char len)
{
    while (len--)
        *to++ = downcase (peek_from_process (from++));
}

//
// Control channel responses
//

void __fastcall__
respond (char code, char * message)
{
    free (response);
    response = malloc (64);
    sprintf (response, "%#02D, %S, 00, 00\r", code, message);
}

void
respond_ok ()
{
    respond (0, "ok");
}

void
respond_syntax_error ()
{
    respond (ERR_SYNTAX, "syntax error");
}

void
respond_file_not_open ()
{
    respond (ERR_FILE_NOT_OPEN, "file not open");
}

void
respond_invalid_command ()
{
    respond (ERR_INVALID_COMMAND, "invalid command");
}


//
// File name parsing
//

// Check if command 'has_prefix' or 'has_params'.
void
analyse_pathname ()
{
    uchar i;
    uchar c;

    has_prefix = has_params = false;

    for (i = 0; i < FNLEN; i++) {
        c = fullname[i];
        if (c == ':')
            has_prefix = true;
        else if (c == ',') {
            has_params = true;
            break;
        }
    }
}

// Copy from 'fullname' to 'prefix', 'pathname' and 'params_list',
// depending on analyse_pathname().
void
split_pathname ()
{
    char *  dest;
    char ** pl;
    uchar   c;
    uchar   i;

    i = 0;
    if (has_prefix) {
        dest = prefix;
        for (; i < FNLEN; i++) {
            c = fullname[i];
            if (c == ':') {
                i++;
                break;
            }
            *dest = c;
        }
    }
    *dest++ = 0;

    dest = pathname;
    for (; i < FNLEN; i++) {
        c = fullname[i];
        if (c == ',') {
            i++;
            break;
        }
        *dest++ = c;
    }
    *dest++ = 0;

    if (has_params) {
        num_params = 0;
        dest = params;
        pl = param_list;
        for (; i < FNLEN; i++) {
            *pl++ = dest;
            num_params++;
            for (; i < FNLEN; i++) {
                c = fullname[i];
                if (c == ',')
                    break;
                *dest++ = c;
            }
            *dest++ = 0;
        }
    }
}

void
parse_pathname ()
{
    analyse_pathname ();
    split_pathname ();
}

// Enter directories along 'pathname'.  If the
// last component is not a directory, the rest
// of the string goes to 'filename', no matter
// if it exists or not.
void
traverse_pathname ()
{
    char * name = pathname;
    char * path;
    upos subdir;
    filename[0] = 0;

    // Get directory to start with.
    directory = ultifs_pwd;
    if (name[0] == '/' && name[1] == '/') {
        directory = ULTIFS_ROOT_DIR;
        name += 2;
    }

    while (1) {
        path = strtok (name, "/");
        while (path) {
            subdir = ultifs_enterdir (directory, path);
            if (subdir == ULTIFS_UNUSED) {
                strcpy (filename, path);
                return;
            }
            directory = subdir;
            path = strtok (NULL, "/");
        }
    }
}

//
// Channel buffers
//

void __fastcall__
clear_buf (channel * ch)
{
    free (ch->buf);
    ch->buf = NULL;
}

void __fastcall__
add_to_buf (channel * ch, void * ptr, size_t len)
{
    if (!ch->buf)
        ch->buf = ch->bufrptr = ch->bufwptr = malloc (2048);
    memcpy (ch->bufwptr, ptr, len);
    ch->bufwptr += len;
}

char __fastcall__
read_from_buf (channel * ch)
{
    char c = *ch->bufrptr++;

    if (ch->bufrptr == ch->bufwptr)
        clear_buf (ch);
    return c;
}

//
// Control channel commands
//

void
cmd_initialize ()
{
    respond_ok ();
}

void __fastcall__
cmd_position ()
{
    channel *  ch;
    upos       p;
    char *     name = fullname;

    if (FNLEN != 5) {
        respond_invalid_command ();
        return;
    }
    if (!(ch = channels[name[1]])) {
        respond_file_not_open ();
        return;
    }
    p = (upos) name[2] | (upos) name[3] << 8 | (upos) name[4] << 16;
    if (p >= ch->file->size) {
        respond (ERR_ILLEGAL_POSITION, "illegal position");
        return;
    }
    ch->file->pos = p;
    respond_ok ();
}

void __fastcall__
change_directory ()
{
    parse_pathname ();
    traverse_pathname ();
    if (filename[0])
        goto invalid;   // Other error: directory does not exist.
    ultifs_pwd = directory;
    respond_ok ();
    return;

invalid:
    respond_invalid_command ();
}

void __fastcall__
make_directory ()
{
    parse_pathname ();
    traverse_pathname ();
    if (!filename[0])
        goto invalid;   // Other error: directory already exists.
    bfile_create_directory (directory, filename);
    respond_ok ();
    return;

invalid:
    respond_invalid_command ();
}

void __fastcall__
commands_c ()
{
    switch (fullname[1]) {
        case 'd':
            change_directory ();
            return;
    }
    respond_syntax_error ();
}

void __fastcall__
commands_m ()
{
    switch (fullname[1]) {
        case 'd':
            make_directory ();
            return;
    }
    respond_syntax_error ();
}

void __fastcall__
open_command ()
{
    channel *  ch = channels[LFN];

    switch (fullname[0]) {
        case 'c':
            commands_c ();
            return;
        case 'i':
            cmd_initialize ();
            return;
        case 'm':
            commands_m ();
            return;
        case 'p':
            cmd_position ();
            return;
    }

    respond_syntax_error ();
    STATUS = 0;
}

void __fastcall__
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
    CLC();
}


//
// Channel allocation
//

channel * __fastcall__
alloc_channel (char * name)
{
    channel * ch = malloc (sizeof (channel));
    if (!ch)
        return NULL;
    ch->sa = SA;
    ch->is_buffered = false;
    ch->buf = NULL;
    ch->file = NULL;
    ch->name = name;
    channels[LFN] = ch;
    return ch;
}

void __fastcall__
free_channel (char lfn)
{
    channel * ch = channels[lfn];
    if (!ch)
        return;
    clear_buf (ch);
    free (ch->name);
    free (ch);
    channels[lfn] = NULL;
}

void
debug (void)
{
}


//
// KERNAL emulation
//

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

    if (FNLEN) {
        copy_from_process (fullname, FNAME, FNLEN);
        fullname[FNLEN] = 0;
    }

    accu = STATUS = 0;
    CLC();
    name = malloc (FNLEN + 1);
    memcpy (name, fullname, FNLEN + 1);
    ch = alloc_channel (name);

    if (SA == 15) {
        ch->is_buffered = true;
        open_command ();
        goto success;
    }

    if (FNLEN == 1 && *name == '$') {
        ch->is_buffered = true;
        make_directory_list (ch);
        goto success;
    }

    parse_pathname ();
    traverse_pathname ();
    if (!params[0] || ((params[0] == 's' || params[0] == 'p') && (!*param_list[1] || *param_list[1] == 'r'))) {
        found_file = ultifs_open (ultifs_pwd, filename, ULTIFS_MODE_READ);
        if (!found_file) {
            respond (ERR_FILE_NOT_FOUND, "file not found");
            goto deverror;
        }

        ch->file = found_file;
        respond_ok ();
        goto success;
    }
    if (*param_list[1] == 'w') {
        if (*param_list[0] != 's' && *param_list[0] != 'p') {
            respond_syntax_error ();
            goto deverror;
        }
        found_file = ultifs_open (ultifs_pwd, filename, ULTIFS_MODE_READ);
        if (found_file) {
            bfile_close (found_file);
            respond (ERR_FILE_EXISTS, "file exists");
            goto deverror;
        }

        ch->file = ultifs_create (ultifs_pwd, filename, *param_list[0] == 's' ?
                                                            CBM_T_SEQ :
                                                            CBM_T_PRG);
        if (!ch->file) {
            respond (ERR_INVALID_COMMAND, "cannot create file");
            return false;
        }
        respond_ok ();
        goto success;
    }
    if (*param_list[1] == 'a') {
        respond (ERR_WRITE_PROTECT_ON, "write protect on");
        goto deverror;
    }
    respond (ERR_INVALID_COMMAND, "invalid command");
    goto deverror;

error:
    SEC();
    return false;

success:
    global_lfns[LFN] = LFN;
    return true;

deverror:
    free_channel (LFN);
    return false;
}

void
ultifs_kclrcn ()
{
}

void
ultifs_kchkin ()
{
    channel * ch = channels[xreg];

    if (!ch) {
        accu = OSERR_FILE_NOT_OPEN;
        goto error;
    }
    if (ch->file && ch->file->mode != ULTIFS_MODE_READ) {
        accu = OSERR_FILE_NOT_IN;
        goto error;
    }
    DFLTN = xreg;
    accu = 0;
    CLC();
    return;

error:
    SEC();
}

void
ultifs_kckout ()
{
    if (!channels[xreg]) {
        accu = OSERR_FILE_NOT_OPEN;
        SEC();
        return;
    }
    if (channels[xreg]->file->mode != ULTIFS_MODE_WRITE) {
        accu = OSERR_FILE_NOT_OUT;
        SEC();
        return;
    }
    DFLTO = xreg;
    accu = 0;
    CLC();
}

char
ultifs_kbasin ()
{
    register channel *  ch = channels[DFLTN];
    register bfile *    file;

    if (!ch) {
        SEC();
        return accu = 0;
    }

    STATUS = 0;
    CLC();

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
        SEC();
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

    if (!ch) {
        SEC();
        STATUS |= STATUS_TIMEOUT_WRITE;
        return;
    }
    if (ch->file->mode != ULTIFS_MODE_WRITE) {
        SEC();
        STATUS |= STATUS_TIMEOUT_WRITE;
        return;
    }

    bfile_write (ch->file, accu);
    STATUS = 0;
    CLC();
}

void
ultifs_kclose ()
{
    channel * ch = channels[accu];

    if (ch->file)
        bfile_close (ch->file);
    free_channel (accu);
    global_lfns[accu] = 0xff;
}

void
ultifs_kclall ()
{
    uchar  i;

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

    CLC();

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
    CLC();

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
