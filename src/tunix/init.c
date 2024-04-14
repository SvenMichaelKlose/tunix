#include <lib/tunix/tunix.h>
#include <conio.h>
#include <stdio.h>
#include <errno.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC     0xe5c3

#define MAX_PROCS   64

char processes[MAX_PROCS];

char
make_baby ()
{
    char pid = tunix_fork ();
    if (pid)
        return pid;
    printf ("Baby %d.\n", tunix_getpid ());
    tunix_suspend (tunix_getpid ());
}

void
test_fork (char nprocs)
{
    char i, mi, pid;

    printf ("Testing %d forked procs.\n", nprocs);

    for (i = 0; i < nprocs; i++) {
        printf ("Fork #%d.\n", i);
        pid = make_baby ();
        if (!pid)
            break;
        processes[i] = pid;
    }

    for (mi = i, i = 0; i < mi; i++) {
        pid = processes[i];
        printf ("Waiting for %d.\n", pid);
        tunix_wait (pid);
    }
}

void
main (void)
{
    char nprocs;

    *(char *) 0x900f = 0x1b;
    //clrscr ();
    printf ("Doing userland test!\n");
    for (nprocs = 1; nprocs < 16; nprocs++)
        test_fork (nprocs);
    printf ("Welcome to TUNIX!");
}
