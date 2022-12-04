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

    last_error[0] = 0;
    if (oserr)
        return 0;

    *p = 0;
    if (oserr = cbm_k_chkin (15)) {
        cbm_k_clrch ();
        printf ("During response read: CHKIN #15: %s\n", os_errors[oserr]);
        exit (-1);
    }
    while (!(cbm_k_readst () & 0x40)) {
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
    if (oserr) {
        printf ("!!! %s\n", os_errors[oserr]);
        exit (-1);
    }
    if (last_error[0])
        printf ("> %s\n", last_error);
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
test_open_missing ()
{
    init_error ();
    oserr = cbm_open (8, device, 8, "0:test,s,r");
    read_error ();
    print_error ();
    cbm_close (8);
    if (oserr || err) {
        if (err != 62) {
            printf ("!!! Error code 62 expected.");
            return;
        }
        err = 0;
        return;
    }
    err = 99;
    printf ("!!! error expected\n");
}

void
chkin (char la)
{
    if (oserr = cbm_k_chkin (la)) {
        cbm_k_clrch ();
        print_error ();
        exit (-1);
    }
}

void
ckout (char la)
{
    if (oserr = cbm_k_ckout (la)) {
        cbm_k_clrch ();
        print_error ();
        exit (-1);
    }
}

void
bsout (char c)
{
    char s;

    cbm_k_bsout (c);
    if (s = cbm_k_readst ()) {
        cbm_k_clrch ();
        printf ("Error status %d", s);
        exit (-1);
    }
}

void
test_create_seq ()
{
    char i;

    init_error ();
    oserr = cbm_open (8, device, 8, "0:test,s,w");
    read_error ();
    if (!oserr && !err) {
        ckout (8);
        for (i = 0; i < 16; i++)
            bsout (i);
    }
    cbm_close (8);
    print_error ();
}

void
test_create_seq_again ()
{
    test_create_seq ();
    if (oserr || err) {
        if (err != 63) {
            printf ("!!! Error code 63 expected.");
            return;
        }
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
    oserr = cbm_open (8, device, 8, "0:test,s,r");
    if (!oserr) {
        chkin (8);
        for (i = 0; i < 16; i++) {
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
    {"Open missing",      test_open_missing},
    {"Create SEQ",        test_create_seq},
    {"Create SEQ again",  test_create_seq_again},
    {"Read SEQ",          test_read_seq}
};

void
main (int argc, char ** argv)
{
    test * p;
    int i;
    int num_tests = sizeof (tests) / sizeof (test);
    int num_errors = 0;

    putchar (0x93);
    if (argc < 2) {
        printf ("Need device number as argument.\n");
        exit (-1);
    }
    device = atoi (argv[1]);

    printf ("FSTEST on device #%d.\n\n", device);

    for (i = 0; i < num_tests; i++) {
        p = &tests[i];
        printf ("%d: %s\n", i + 1, p->description);
        oserr = err = last_error[0] = 0;
        p->fun ();
        if (oserr || err) {
            printf ("!!! Test failed.\n");
            num_errors++;
        }
    }

    printf ("Found %d error%s.\n", num_errors, num_errors == 1 ? "" : "s");
}
