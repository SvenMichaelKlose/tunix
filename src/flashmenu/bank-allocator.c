#include <string.h>

#define NUM_BANKS   (1024 / 8)

char allocated_banks[NUM_BANKS / 8];

void
init_bank_allocator ()
{
    bzero (allocated_banks, sizeof (allocated_banks));
    allocated_banks[0] = 255;   // TODO: Missing some free banks here.
}

char
alloc_bank ()
{
    char i;
    char byte;
    char bit;

    for (i = 1; i < NUM_BANKS; i++) {
        byte = i >> 3;
        bit = 1 << (i & 7);
        if (!(allocated_banks[byte] & bit)) {
            allocated_banks[byte] |= bit;
            return i;
        }
    }

    return 0;
}

void __fastcall__
free_bank (char i)
{
    allocated_banks[i >> 3] &= ~(1 << (i & 7));
}
