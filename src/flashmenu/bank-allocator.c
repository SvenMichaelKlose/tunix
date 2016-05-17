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
    unsigned int i;

    for (i = 0; i < NUM_BANKS; i++)
        if (allocated_banks[i / 8] & 1 << i % 8) {
            allocated_banks[i / 8] |= 1 << i % 8;
            return i;
        }

    return 0;
}

void __fastcall__
free_bank (char i)
{
    allocated_banks[i / 8] &= (1 << i % 8) ^ 255;
}
