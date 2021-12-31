#include <cbm.h>

#include <lib/ultimem/ultimem.h>

#include "ultifs.h"
#include "wrap-ultifs.h"

#define ULTIFS_BANK     5

char
w_ultifs_mount ()
{
    unsigned oldbank = *ULTIMEM_BLK1;
    char r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = ultifs_mount ();
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

bfile * __fastcall__
w_ultifs_open (upos directory, char * name, char mode)
{
    unsigned oldbank = *ULTIMEM_BLK1;
    bfile * r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = ultifs_open (directory, name, mode);
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

int __fastcall__
w_bfile_readm (bfile * b, char * bytes, unsigned len)
{
    unsigned oldbank = *ULTIMEM_BLK1;
    int r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = bfile_readm (b, bytes, len);
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

void __fastcall__
w_bfile_close (bfile * b)
{
    unsigned oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    bfile_close (b);
    *ULTIMEM_BLK1 = oldbank;
}

char
w_ultifs_opendir ()
{
    unsigned oldbank = *ULTIMEM_BLK1;
    char r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = ultifs_opendir ();
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

char __fastcall__
w_ultifs_readdir (struct cbm_dirent * dirent)
{
    unsigned oldbank = *ULTIMEM_BLK1;
    char r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = ultifs_readdir (dirent);
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

void
w_ultifs_closedir ()
{
    unsigned oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    ultifs_closedir ();
    *ULTIMEM_BLK1 = oldbank;
}

char __fastcall__
w_ultifs_enterdir (char * name)
{
    unsigned oldbank = *ULTIMEM_BLK1;
    char r;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    r = ultifs_enterdir (name);
    *ULTIMEM_BLK1 = oldbank;

    return r;
}

void
w_ultifs_leavedir ()
{
    unsigned oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = ULTIFS_BANK;
    ultifs_leavedir ();
    *ULTIMEM_BLK1 = oldbank;
}
