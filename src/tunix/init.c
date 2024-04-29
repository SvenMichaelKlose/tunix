#include <lib/tunix/tunix.h>
#include <stdlib.h>
#include <conio.h>
#include <stdio.h>
#include <errno.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC     0xe5c3

#define MAX_PROCS   32

#define ALLOC_AROUND_FORK   1

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
    char i, mi, bank;

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
    printf ("\nIntermediate status:\n");
    tunix_proc_list ();
    tunix_mem_info ();

    printf ("\n### Freeing.\n");
    mi = i;
    for (i = 0; i < mi; i++) {
        bank = banks[i];
        printf ("$%02x ", bank);
        tunix_free (bank);
    }

    printf ("\n## Done. Max %d banks.\n", mi);
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
#if ALLOC_AROUND_FORK
    char nbanks_a = test_alloc0 (3);
    char nbanks_b;
#endif
    char i, mi, pid;

    printf ("## %d children at once.\n",
            nprocs);
    tunix_mem_info ();

    for (i = 0; i < nprocs; i++) {
        tunix_mode (0);
        printf ("Forking %d.\n", i + 1);
        tunix_mode (1);
        pid = make_baby (rand () % nprocs + 1);
        tunix_mem_info ();
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
        tunix_wait (pid);
        tunix_mode (0);
        printf ("$%02x exited.\n", pid);
        tunix_mode (1);
    }
    printf ("\n");
    tunix_proc_info (tunix_getpid ());
    tunix_mem_info ();

#if ALLOC_AROUND_FORK
    nbanks_b = test_alloc0 (4);
    if (nbanks_a != nbanks_b) {
        tunix_mem_info ();
        tunix_proc_list ();
        printf ("! %d banks missing "
                "after fork test "
                "(round %d)).",
                nbanks_a - nbanks_b,
                nprocs);
        // TODO: Wait for keypress.
        while (1);
        tunix_exit (-1);
    }
    printf ("## %d children done.\n",
            nprocs);
#endif
}

void
test_forks (char maxprocs)
{
    char nprocs;

    printf ("# Forks\n");
    printf ("Initial status:\n");
    tunix_proc_list ();
    tunix_mem_info ();

    for (nprocs = 0;
         nprocs < maxprocs;
         nprocs++)
        test_fork (nprocs + 1);
}

void
main (void)
{
    printf ("Doing userland tests\n");
    printf ("Initial process list:\n");
    tunix_proc_list ();
    tunix_mem_info ();

    test_alloc ();
    test_forks (MAX_PROCS);
    test_forks (MAX_PROCS);

    printf ("Welcome to TUNIX!");
}
