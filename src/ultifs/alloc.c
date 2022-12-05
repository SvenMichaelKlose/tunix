// Memory bank allocator

#include <stddef.h>
#include <stdlib.h>

#define NUM_BANKS   128
#define FIRST_BANK  6
#define LAST_BANK   126

char banks[NUM_BANKS];

#define PUSH(l, x) x->next = l; l = x;
#define POP(to, l)  to = l; l = l->next;

typedef struct _bank {
    struct _bank *  next;
    char  num;
} bank;

#pragma bss-name (push, "ZEROPAGE")
bank * free_banks;
bank * allocated_banks;
bank * b;
#pragma zpsym("free_banks")
#pragma zpsym("allocated_banks")
#pragma zpsym("b")
#pragma bss-name (pop)

void
init_alloc ()
{
    int i;

    allocated_banks = NULL;

    free_banks = calloc (1, sizeof (bank) * NUM_BANKS);
    for (i = FIRST_BANK; i <= LAST_BANK; i++)
        free_banks[i].num = i;
    for (i = FIRST_BANK; i <= LAST_BANK - 1; i++)
        free_banks[i].next = &free_banks[i + 1];
}

char
alloc_bank ()
{
    if (!free_banks)
        return 0;
    POP(b, free_banks);
    PUSH(b, allocated_banks);
    banks[b->num] = 1;
    return b->num;
}

char __fastcall__
free_bank (char num)
{
    if (!banks[num])
        return -1;
    POP(b, allocated_banks);
    b->num = num;
    PUSH(b, free_banks);
    banks[num] = 0;
    return 0;
}
