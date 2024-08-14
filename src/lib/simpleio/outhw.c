#include <stdbool.h>
#include <stddef.h>
#include <simpleio/libsimpleio.h>

void FASTCALL
outhw (unsigned v)
{
    outhb (v >> 8);
    outhb (v & 255);
}
