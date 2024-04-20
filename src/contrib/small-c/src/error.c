#include <stdio.h>
#include "defs.h"
#include "data.h"

error (char msg)
{
    FILE *tempfile;

    tempfile = output;
    output = stdout;
    doerror (msg);
    output = tempfile;
    doerror (msg);
    errcnt++;
}

doerror (char *msg)
{
    int k;
    // print actual source filename 
    if (finame[inclsp]) {
        output_string (finame[inclsp]);
    }
    output_string (":");
    // print source line number 
    output_decimal (srcln[inclsp]);
    output_string (":");
    // print column number 
    output_decimal (lptr);
    output_string (": error: ");
    output_string (msg);
    newline ();
    output_string (line);
    newline ();
    k = 0;
    while (k < lptr) {
        if (line[k] == 9)
            print_tab ();
        else
            output_byte (' ');
        k++;
    }
    output_byte ('^');
    newline ();
}
