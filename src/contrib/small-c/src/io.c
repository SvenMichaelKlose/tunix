#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"

openin (char *p)
{
    strcpy (fname, p);
    fixname (fname);
    if (!checkname (fname))
        return NO;
    if ((input = fopen (fname, "r")) == NULL) {
        pl ("Open failure\n");
        return NO;
    }
    kill ();
    return YES;
}

openout ()
{
    outfname (fname);
    if ((output = fopen (fname, "w")) == NULL) {
        pl ("Open failure");
        return NO;
    }
    kill ();
    return YES;
}

// Make output name from input name.
outfname (char *s)
{
    while (*s)
        s++;
    *--s = 'o';
}

// Terminate string at line feed.
fixname (char *s)
{
    while (*s && *s++ != LF);
    if (!*s)
        return;
    *(--s) = 0;
}

// Check that filename is "*.c".
checkname (char *s)
{
    while (*s)
        s++;
    if (*--s != 'c')
        return NO;
    if (*--s != '.')
        return NO;
    return YES;
}

kill ()
{
    lptr = 0;
    line[lptr] = 0;
}

readline ()
{
    int k;
    FILE *unit;

    FOREVER {
        if (feof (input))
            return;
        if ((unit = input2) == NULL)
            unit = input;
        kill ();
        while ((k = fgetc (unit)) != EOF) {
            if (k == CR || k == LF || lptr >= LINEMAX)
                break;
            line[lptr++] = k;
        }
        line[lptr] = 0;
        // increment source line number
        // of actual file 
        srcln[inclsp]++;
        if (k <= 0)
            if (input2 != NULL) {
                input2 = inclstk[--inclsp];
                fclose (unit);
            }
        // Write source line as comment.
        if (lptr) {
            if (ctext & cmode)
                gen_srcline (line);
            lptr = 0;
            return;
        }
    }
}

inbyte ()
{
    while (!ch ()) {
        if (feof (input))
            return 0;
        preprocess ();
    }
    return gch ();
}

inchar ()
{
    if (!ch ())
        readline ();
    if (feof (input))
        return 0;
    return gch ();
}

// Gets current char from input line and
// moves to the next one.
gch ()
{
    if (!ch ())
        return 0;
    return line[lptr++] & 127;
}

// Next char.
nch ()
{
    if (!ch ())
        return 0;
    return line[lptr + 1] & 127;
}

// Current char.
ch ()
{
    return line[lptr] & 127;
}

// Print a carriage return and a string
// only to console.
pl (char *str)
{
    int k;

    k = 0;
    putchar (LF);
    while (str[k])
        putchar (str[k++]);
}
