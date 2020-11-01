#include <stdio.h>
#include <cbm.h>

extern void __fastcall__ init_secondary_wedge (char rom_device);
extern void __fastcall__ init_kernal_emulation (void);

void
list_directory (char device)
{
    static struct cbm_dirent dirent;

    cbm_opendir (2, device, "$");
    while (!cbm_readdir (2, &dirent))
        printf ("%s\n", dirent.name);
    cbm_closedir (2);
}

void
main ()
{
    char device = 12;
    printf ("UltiFS wedge\n");
    printf ("Flash ROM device: %d\n", device);

    init_secondary_wedge (device);
    init_kernal_emulation ();
    list_directory (8);
}
