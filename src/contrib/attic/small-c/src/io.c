#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"
#include "preproc.h"
#include "ir.h"
#include "io.h"

int
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

int
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
void
outfname (char *s)
{
    while (*s)
        s++;
    *--s = 'o';
}

// Terminate string at line feed.
void
fixname (char *s)
{
    while (*s && *s++ != LF);
    if (!*s)
        return;
    *(--s) = 0;
}

// Check that filename is "*.c".
int
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

void
kill ()
{
    lptr = 0;
    line[lptr] = 0;
}

void
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
        if (k <= 0 && input2 != NULL) {
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

char
inbyte ()
{
    while (!ch ()) {
        if (feof (input))
            return 0;
        preprocess ();
    }
    return gch ();
}

char
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
char
gch ()
{
    if (!ch ())
        return 0;
    return line[lptr++];
}

// Next char.
char
nch ()
{
    return ch () ? line[lptr + 1] : 0;
}

// Current char.
char
ch ()
{
    return line[lptr];
}

// Print a carriage return and a string
// only to console.
void
pl (char *str)
{
    putchar (LF);
    puts (str);
}
