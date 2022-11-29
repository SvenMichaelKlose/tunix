// UltiFS
//
// Top-level
//
// Author: Sven Michael Klose <pixel@hugbox.org>


#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <lib/ultimem/ultimem.h>

//extern void __fastcall__ init_primary_wedge (void * start);
//extern void __fastcall__ init_secondary_wedge (char rom_device);
extern void __fastcall__ init_hooks (void);
extern void __fastcall__ init_kernal_emulation (void);


// Get us out of the standard banks where the next app will
// be running.  The primary wedge will bank in BLK1 for the
// secondary wedge and that one will bank in BLK2 and BLK3.
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
extern char driver_banks[32];
extern void init_secondary_wedge (void);

void
main (void)
{
    init_secondary_wedge ();
    init_kernal_emulation ();
    init_hooks ();
    copy_program_to_resident_banks (117); // TODO: Use bank allocator!
    driver_banks[12] = 117;
    reset ();
}
