#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char device = 12;

void
test_new_disk ()
{
    cbm_open (15, device, 15, "N0:TESTDISK,01");
}

typedef void (*voidfun) ();

typedef struct _test {
    const char *  description;
    voidfun       fun;
} test;

test tests[] = {
    {"New disk", test_new_disk}
};

void
main (void)
{
    test * p;
    int i;
    int num_tests = sizeof (tests) / sizeof (test);

    printf ("Filesystem tests: %d\n", num_tests);

    for (i = 0; i < num_tests; i++) {
        p = &tests[i];
        printf ("Test %d: %s\n", i + 1, p->description);
        p->fun ();
    }
}
