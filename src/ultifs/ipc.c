#pragma code-name ("ULTIFS")

#include <stdbool.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/ultimem/ultimem.h>

#include "ultifs.h"
#include "kernal.h"

char __fastcall__
peek_from_process (char * p)
{
    unsigned char ph = (unsigned) p >> 8;

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
    register unsigned char ph = (unsigned) p >> 8;

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
