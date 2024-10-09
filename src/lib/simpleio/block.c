#ifdef __CC65__
#ifndef __CBM__
    #define __CBM__
#endif
#include <cbm.h>
#include <ingle/cc65-charmap.h>
#endif // #ifdef __CC65__

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include "libsimpleio.h"

size_t len;

void FASTCALL
inm (char * p, size_t nbytes)
{
    for (len = nbytes; len > 0; len--)
        *p++ = in ();
}
