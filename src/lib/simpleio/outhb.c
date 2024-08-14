#include <stdbool.h>
#include <stddef.h>
#include <simpleio/libsimpleio.h>

void FASTCALL
outhb (unsigned char v)
{
    outhn (v >> 4);
    outhn (v & 15);
}
