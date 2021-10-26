#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>

extern void __fastcall__ init_secondary_wedge (char rom_device);
extern void __fastcall__ init_kernal_emulation (void);

extern char blk2;
extern char blk3;

void
list_directory (char device)
{
    static struct cbm_dirent dirent;

    printf ("Directory %d:\n", device);

    cbm_opendir (8, device, "$");
    while (!cbm_readdir (8, &dirent))
        cputs (dirent.name);
    cbm_closedir (8);
}

void
dump_file (char device, char sfn, char * name)
{
    char * data = malloc (1024);

    clrscr ();
    printf ("dump %d, %d, %s\n", device, sfn, name);
    gotoxy (0, 2);

    cbm_open (8, device, sfn, name);
    cbm_read (8, data, 1024);
    cbm_close (8);
cputs (data);

    free (data);
}

void
main ()
{
    char device = 12;

    printf ("UltiFS wedge\n");
    printf ("Flash ROM device: %d\n", device);

    init_secondary_wedge (device);
    init_kernal_emulation ();

    list_directory (12);
    //dump_file (8, 8, "main.c");
    //dump_file (12, 15, "AB");
    cputs ("Finished.\n");
}
