#include <lib/tunix/tunix.h>
#include <conio.h>
#include <stdio.h>
#include <errno.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC     0xe5c3

#define MAX_PROCS   64

char processes[MAX_PROCS];
char banks[256];

char
test_alloc0 (char phase)
{
    int i, mi;
    char bank;

    printf ("## (Round %d.)\n",
            phase);

    printf ("### Allocating.\n");
    for (i = 0; i < 156; i++) {
        bank = tunix_alloc ();
        if (!bank) {
            printf ("\n%d banks allocated.\n", i);
            break;
        }
        banks[i] = bank;
        printf ("$%02x ", bank);
    }

    printf ("### Freeing.\n");
    for (mi = i, i = 0; i < mi; i++) {
        bank = banks[i];
        printf ("$%02x ", bank);
        tunix_free (bank);
    }

    printf ("\n## Done.\n");
    return mi;
}

void
test_alloc (void)
{
    char nbanks_a = test_alloc0 (1);
    char nbanks_b = test_alloc0 (2);
    if (nbanks_a != nbanks_b) {
        printf ("! Missing %d banks ",
                "to allocate in second "
                "round.\n",
                nbanks_a - nbanks_b);
        tunix_exit (-1);
    }
    printf ("Test passed.\n");
}

char
make_baby ()
{
    char i;
    char pid = tunix_fork ();
    if (pid)
        return pid;
    for (i = 0; i < 10; i++)
        tunix_schedule ();
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

    printf ("## %d procs at once.\n",
            nprocs);

    for (i = 0; i < nprocs; i++) {
        tunix_mode (0);
        printf ("Forking %d.\n", i + 1);
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
    printf ("\n");
}

void
test_forks (char maxprocs)
{
    char nprocs;

    printf ("# Forks\n");

    for (nprocs = 0;
         nprocs < maxprocs;
         nprocs++)
        test_fork (nprocs + 1);
}

void
main (void)
{
    printf ("Doing userland test!\n");

    test_alloc ();
    test_forks (4);
    test_forks (4);

    printf ("Welcome to TUNIX!");
}
