// TUNIX UltiMem Flash Boot

#include <stdio.h>

#include <lib/ultimem/ultimem.h>

#pragma code-name (push, "TRAMPOLINE")

void __fastcall__
boot (char bank)
{
    *ULTIMEM_BLK5 = bank;
    ((void (*)(void))0xa00c)();
}

#pragma code-name (pop)

void
main (void)
{
    *ULTIMEM_CONTROL = ULTIMEM_CTRL_LED;
printf ("HELLO WORLD!");
    boot ((*ULTIMEM_CONTROL & ULTIMEM_CTRL_SWITCH1) ? 1 : 127);
}
