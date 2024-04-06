// TUNIX UltiMem Flash Boot

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
    ultimem_unhide ();
    *ULTIMEM_CONTROL = ULTIMEM_CTRL_LED;
    *ULTIMEM_CONFIG1 = 0x3f;
    *ULTIMEM_CONFIG2 = 0xff;
    boot ((*ULTIMEM_CONTROL & ULTIMEM_CTRL_SWITCH1) ? 1 : 127);
}
