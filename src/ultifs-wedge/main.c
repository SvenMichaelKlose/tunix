#include <stdio.h>
#include <stdlib.h>

#include <cbm.h>

extern void __fastcall__ init_secondary_wedge (char rom_device);
extern void __fastcall__ init_kernal_emulation (void);

extern char blk2;
extern char blk3;

void
list_directory (char device)
{
    static struct cbm_dirent dirent;

    cbm_opendir (8, device, "$");
    while (!cbm_readdir (8, &dirent))
        printf ("%s\n", dirent.name);
    cbm_closedir (8);
}

void
dump_file (char device, char sfn, char * name)
{
    char * data = malloc (1024);
    unsigned len;

    cbm_open (8, device, sfn, name);
    while ((len = cbm_read (8, data, 1024)) > 0)
        printf ("%d ", len);
    cbm_close (8);
}

void
main ()
{
    char device = 12;
    printf ("UltiFS wedge\n");
    printf ("Flash ROM device: %d\n", device);

    init_secondary_wedge (device);
    init_kernal_emulation ();
    //list_directory (8);
    dump_file (12, 15, NULL);
}
