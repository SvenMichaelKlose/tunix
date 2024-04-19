#include <stdio.h>
#include "defs.h"
#include "data.h"

error (ptr)
char ptr[];
{
    FILE *tempfile;

    tempfile = output;
    output = stdout;
    doerror (ptr);
    output = tempfile;
    doerror (ptr);
    errcnt++;
}

doerror (ptr)
char *ptr;
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
    output_string (ptr);
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
