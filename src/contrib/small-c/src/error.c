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
        outs (finame[inclsp]);
    }
    outs (":");
    // print source line number 
    outn (srcln[inclsp]);
    outs (":");
    // print column number 
    outn (lptr);
    outs (": error: ");
    outs (msg);
    newline ();
    outs (line);
    newline ();
    k = 0;
    while (k < lptr) {
        if (line[k] == 9)
            print_tab ();
        else
            outb (' ');
        k++;
    }
    outb ('^');
    newline ();
}
