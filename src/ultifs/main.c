#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <lib/ultimem/ultimem.h>
#include <lib/log/liblog.h>

#include "ultifs.h"

#ifdef TEST
#include "test.h"
#endif

extern void __fastcall__ init_dispatcher (void);
extern void __fastcall__ init_kernal_emulation (void);

void
copy_program_to_resident_banks (unsigned first_bank)
{
    char      old_cfg2 = *ULTIMEM_CONFIG2;
    unsigned  old_blk5 = *ULTIMEM_BLK5;

    // r/w RAM on BLK5.
    *ULTIMEM_CONFIG2 = *ULTIMEM_CONFIG2 & 0x3f | (ULTIMEM_CFG_RW_RAM << 6);

    *ULTIMEM_BLK5 = first_bank++;
    memcpy ((void *) 0xa000, (void *) 0x2000, 0x2000);
    *ULTIMEM_BLK5 = first_bank++;
    memcpy ((void *) 0xa000, (void *) 0x4000, 0x2000);
    *ULTIMEM_BLK5 = first_bank;
    memcpy ((void *) 0xa000, (void *) 0x6000, 0x2000);

    *ULTIMEM_CONFIG2 = old_cfg2;
    *ULTIMEM_BLK5    = old_blk5;
}

void reset (void);
extern void init_secondary_wedge (void);

void
main (void)
{
#ifdef TEST
    test ();
#endif
    init_log_message ();
    ultifs_mount ();
    init_secondary_wedge ();
    init_kernal_emulation ();
    init_dispatcher ();
    copy_program_to_resident_banks (117); // TODO: Use bank allocator!
    reset ();
}
