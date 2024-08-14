#include <stdbool.h>
#include <stddef.h>
#include <simpleio/libsimpleio.h>

void FASTCALL
outhn (unsigned char v)
{
    v &= 15;
    if (v > 9)
        v = v - 10 + 'a';
    else
        v += '0';
    out (v);
}
