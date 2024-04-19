#include <lib/tunix/tunix.h>
#include <conio.h>
#include <stdio.h>
#include <errno.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC     0xe5c3

#define MAX_PROCS   64

char processes[MAX_PROCS];
char banks[256];

extern void debug (void);
void
debug (void)
{
}

char
test_alloc0 (char round)
{
    int i, mi;
    char bank;

    printf ("## (Round %d.)\n", round);

    printf ("### Allocating.\n");
    for (i = 0; i < 128; i++) {
        bank = tunix_alloc ();
        if (!bank)
            break;
        banks[i] = bank;
        printf ("$%02x ", bank);
    }
    printf ("\n%d banks allocated.", i);

    printf ("\n### Freeing.\n");
    for (mi = i, i = 0; i < mi; i++) {
        bank = banks[i];
        printf ("$%02x ", bank);
        tunix_free (bank);
    }

    printf ("\n## Done.\n");
    return mi;
}

char
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
    return nbanks_a;
}

char
make_baby (char schedule_rounds)
{
    char i, pid;

    pid = tunix_fork ();
    tunix_mode (1);
    if (pid > 0)
        return pid;
    if (pid == 255)
        return 0;
    for (i = 0;
         i < schedule_rounds;
         i++)
        tunix_schedule ();
    tunix_exit (0);
}

void
test_fork (char nprocs)
{
    char nbanks_a = test_alloc0 (3);
    char nbanks_b;
    char i, mi, pid;

    printf ("## %d children at once.\n",
            nprocs);

    for (i = 0; i < nprocs; i++) {
        tunix_mode (0);
        printf ("Forking %d.\n", i + 1);
        tunix_mode (1);
        pid = make_baby (0);
        if (pid < 1)
            break;
        processes[i] = pid;
        tunix_mode (0);
        printf ("Forked $%02x.\n", pid);
        tunix_proc_info (pid);
        tunix_mode (1);
    }

    tunix_mode (0);
    printf ("%d children total.\n", i);
    tunix_proc_list ();
    tunix_mode (1);

    for (mi = i, i = 0; i < mi; i++) {
        pid = processes[i];
        tunix_mode (0);
        printf ("Waiting for $%02x.\n",
                pid);
        tunix_mode (1);
        //if (nprocs == 2 && i == 0) debug ();
        tunix_wait (pid);
        tunix_mode (0);
        printf ("$%02x exited.\n", pid);
        tunix_mode (1);
    }
    printf ("\n");

    nbanks_b = test_alloc0 (4);
    if (nbanks_a != nbanks_b) {
        tunix_mode (0);
        tunix_mem_info ();
        printf ("! %d banks missing "
                "after fork test "
                "(round %d)).",
                nbanks_a - nbanks_b,
                nprocs);
        while (1);
        tunix_exit (-1);
    }
    printf ("## %d children done.\n",
            nprocs);
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
