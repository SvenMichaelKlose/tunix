#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>

extern void __fastcall__ init_primary_wedge (void);
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
        //printf ("%s\n", dirent.name);
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

typedef void voidfun (void);

#define MEMHIGH     (*(unsigned *) 0x0283)
#define FA      (*(char*)  0xba)

void
main ()
{
    char device = 12;

    printf ("UltiMem ROM disk\n");
    printf ("by Sven Michael Klose\n");
    printf ("   <pixel@hugbox.org>\n");
    printf ("   27 DEC 2021\n");

    init_primary_wedge ();
    init_secondary_wedge (device);
    init_kernal_emulation ();

    printf ("Flash ROM mounted on device %d.\n", device);
    MEMHIGH = 0x1fff;

    ((voidfun*) 0xe378) ();

    list_directory (12);
    //dump_file (8, 8, "main.c");
    //dump_file (12, 15, "AB");
    cputs ("Finished.\n");
}
