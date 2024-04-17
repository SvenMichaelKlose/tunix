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
    tunix_mode (0);
    tunix_exit (0);
}

extern void debug (void);
void
debug (void)
{
}

void
test_fork (char nprocs)
{
    char i, mi, pid;

    printf ("*** %d procs at once.\n",
            nprocs);

    for (i = 0; i < nprocs; i++) {
        tunix_mode (0);
        printf ("Forking #%d.\n", i + 1);
        tunix_mode (1);
        //if (nprocs == 1 && i == 0)
            //debug ();
        pid = make_baby ();
        //if (!pid) break;
        processes[i] = pid;
        tunix_mode (0);
        printf ("Forked $%02x.\n", pid);
        tunix_proc_info (pid);
        tunix_mode (1);
    }

    for (mi = i, i = 0; i < mi; i++) {
        pid = processes[i];
        tunix_mode (0);
        printf ("Waiting for $%02x.\n", pid);
        tunix_mode (1);
        tunix_wait (pid);
        tunix_mode (0);
        printf ("$%02x exited.\n", pid);
        tunix_mode (1);
    }
    printf ("***\n");
}

void
main (void)
{
    char nprocs;

    printf ("Doing userland test!\n");
    for (nprocs = 1;
         nprocs < 3;
         nprocs++)
        test_fork (nprocs);
    printf ("Welcome to TUNIX!");
}
