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

char oserr;
char err;

int
read_error ()
{
    char c;
    char * p = last_error;

    if (oserr)
        return 0;

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

    return err = (last_error[0] - '0') * 10 + last_error[1] - '0';
}

void
print_error ()
{
    if (oserr)
        printf ("! %s\n", os_errors[oserr]);
    printf ("%s\n", last_error);
}

void
send_command (char * cmd)
{
    oserr = cbm_open (15, device, 15, cmd);
    read_error ();
    cbm_close (15);
    print_error ();
}

void
test_initialize ()
{
    send_command ("i0");
}

void
test_new_disk ()
{
    send_command ("n0:TESTDISK,01");
}

typedef void (*voidfun) ();

typedef struct _test {
    const char *  description;
    voidfun       fun;
} test;

test tests[] = {
    {"Initialize", test_initialize},
    {"New disk", test_new_disk}
};

void
main (void)
{
    test * p;
    int i;
    int num_tests = sizeof (tests) / sizeof (test);

    printf ("%d filesystem tests on #%d\n", num_tests, device);

    for (i = 0; i < num_tests; i++) {
        p = &tests[i];
        printf ("Test %d: %s\n", i + 1, p->description);
        p->fun ();
    }
}
