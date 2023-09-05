#pragma code-name ("ULTIFS")

//#include <lib/ingle/cc65-charmap.h>

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ultifs.h"
#include "kernal.h"
#include "test.h"

#define NAME    "0:fnord,foo,bar"

void
issue (char * msg)
{
    printf ("\n");
    printf (msg);
    exit (-1);
}

void
test ()
{
    printf ("Self-test: ");
    strcpy (fullname, NAME);
    FNLEN = strlen (NAME);
    analyse_pathname ();
    split_pathname ();
    if (strcmp (pathname, "fnord"))
        issue ("Incorrect 'pathname'.");
    if (num_params != 2)
        issue ("'num_params' is not 2.");
    if (strcmp (params, "foo"))
        issue ("1st param is not 'foo'.");
    printf ("OK.");
}
