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

void
init_error ()
{
    cbm_open (15, device, 15, NULL);
}

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

    return err = (last_error[0] - '0') * 10 + last_error[1] - '0';
}

void
print_error ()
{
    cbm_k_close (15);
    if (oserr)
        printf ("! %s\n", os_errors[oserr]);
    if (last_error[0])
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

void
test_create_seq ()
{
    char i;

    init_error ();
    oserr = cbm_open (8, device, 8, "0:test,s,w");
    read_error ();
    if (!oserr && !err) {
        cbm_k_ckout (8);
        for (i = 1; i < 255; i++)
            cbm_k_bsout (i);
    }
    cbm_close (8);
    print_error ();
}

void
test_create_seq_again ()
{
    test_create_seq ();
    if (oserr | err) {
        err = 0;
        return;
    }
    err = 99;
    printf ("!!! error expected\n");
}

void
test_read_seq ()
{
    char i;
    char v;

    init_error ();
    oserr = cbm_open (8, device, 8, "test,s,r");
    if (!oserr) {
        cbm_k_chkin (8);
        for (i = 1; i < 255; i++) {
            v = cbm_k_basin ();
            if (v != i) {
                strcpy (last_error, "99 read error");
                break;
            }
        }
    }
    read_error ();
    cbm_close (8);
    print_error ();
}

typedef void (*voidfun) ();

typedef struct _test {
    const char *  description;
    voidfun       fun;
} test;

test tests[] = {
    {"Initialize",        test_initialize},
    //{"New disk",    test_new_disk},
    {"Create SEQ",        test_create_seq},
    {"Create SEQ again",  test_create_seq_again},
    {"Read SEQ",          test_read_seq}
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
        printf ("%d: %s\n", i + 1, p->description);
        oserr = err = last_error[0] = 0;
        p->fun ();
    }
}
