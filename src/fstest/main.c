#include <cbm.h>
#include <conio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char device = 8;

char last_error[256];

char * os_errors[] = {
    "No error.",
    "Too many open files.",
    "File already open.",
    "File not open.",
    "File not found.",
    "Device not present.",
    "File not in.",
    "File not out.",
    "Missing file name.",
    "Illegal device number."
};

int
read_error ()
{
    char c;
    char * p = last_error;

    *p = 0;
    cbm_k_chkin (15);
    while (1) {
        c = cbm_k_basin ();
        if (c == 0x0d) {
            *p++ = 0;
            break;
        }
        *p++ = c;
    }
    cbm_k_clrch ();
}

void
test_new_disk ()
{
    char err;

    err = cbm_open (15, device, 15, "N0:TESTDISK,01");
    if (!err)
        read_error ();
    cbm_close (15);

    if (err)
        printf ("! %s\n", os_errors[err]);
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
