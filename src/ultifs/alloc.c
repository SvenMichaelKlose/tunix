#include <stddef.h>
#include <stdlib.h>

#define NUM_BANKS   128
#define FIRST_BANK  6
#define LAST_BANK   126

char banks[NUM_BANKS];

#define PUSH(l, x) banks[x] = l; l = x;
#define POP(to, l) to = l; l = banks[l];

#pragma bss-name (push, "ZEROPAGE")
char free_banks;
char allocated_banks;
char b;
char i;
#pragma zpsym("free_banks")
#pragma zpsym("allocated_banks")
#pragma zpsym("b")
#pragma bss-name (pop)

void
init_alloc ()
{
    allocated_banks = 0;
    free_banks = FIRST_BANK;

    for (i = FIRST_BANK; i < LAST_BANK - 1; i++)
        banks[i] = i + 1;
}

char
alloc_bank ()
{
    if (!free_banks)
        return 0;
    POP(b, free_banks);
    PUSH(b, allocated_banks);
    return b;
}

char __fastcall__
free_bank (char num)
{
    if (!banks[num])
        return -1;
    POP(b, allocated_banks);
    PUSH(b, free_banks);
    return 0;
}
