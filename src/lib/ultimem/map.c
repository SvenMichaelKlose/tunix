#include "ultimem.h"

void * __fastcall__
ultimem_map_ptr (void * base, unsigned short * reg, void * ptr, unsigned short * blkregs)
{
    unsigned bptr = (unsigned) ptr;
    char ri;

    if (bptr < 0x2000 || bptr > 0x7fff)
        return (void *) bptr;

    bptr -= 0x2000;
    ri = (unsigned) bptr >> 14;
    *reg = blkregs[ri];

    return (void *) ((bptr & 0x1fff) + (unsigned) base);
}
