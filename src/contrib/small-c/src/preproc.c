#include <stdio.h>
#include <string.h>
#include <error.h>

#include "defs.h"
#include "data.h"
#include "io.h"
#include "gen.h"
#include "lex.h"
#include "sym.h"
#include "preproc.h"

char
keepch (char c)
{
    mline[mptr] = c;
    if (mptr < MPMAX)
        mptr++;
    return c;
}

void
toggle (char name, int onoff)
{
    switch (name) {
    case 'C':
        ctext = onoff;
        break;
    }
}

char
remove_one_line_comment (char c)
{
    if (c == '/' && ch () == '/') {
        while (gch ());
        return 0;
    }
    return c;
}

char
putmac (char c)
{
    macq[macptr] = c;
    if (macptr < MACMAX)
        macptr++;
    return c;
}

void
addmac ()
{
    char sname[NAMESIZE];
    int k;
    int mp;

    if (!symname (sname)) {
        illname ();
        kill ();
        return;
    }
    if (mp = findmac (sname)) {
        perror ("Duplicate define");
        delmac (mp);
    }
    k = 0;
    while (putmac (sname[k++]));
    while (ch () == ' ' | ch () == 9)
        gch ();
    while (putmac (remove_one_line_comment (gch ())));
    if (macptr >= MACMAX)
        perror ("macro table full");

}

void
delmac (int mp)
{
    // step over previous NULL. 
    --mp;
    --mp;
    while (mp >= 0 && macq[mp])
        macq[mp--] = '%';
}

int
findmac (char *sname)
{
    int k;

    k = 0;
    while (k < macptr) {
        if (astreq (sname, macq + k, NAMEMAX)) {
            while (macq[k++]);
            return k;
        }
        while (macq[k++]);
        while (macq[k++]);
    }
    return 0;
}

// Remove "brackets" surrounding
// include file name.  Ssee DEFLIB.
FILE *
fix_include_name ()
{
    char c1, c2, *p, *ibp;
    char buf[20];
    FILE *fp;
    char buf2[100];

    ibp = &buf[0];

    if ((c1 = gch ()) != '"' && c1 != '<')
        return NULL;
    for (p = line + lptr; *p;)
        *ibp++ = *p++;
    c2 = *(--p);
    if (c1 == '"' && c2 != '"' || c2 != '>') {
        perror ("incorrect delimiter");
        return 0;
    }
    *--ibp = 0;
    fp = NULL;
    if (c1 == '<' || !(fp = fopen (buf, "r"))) {
        strcpy (buf2, DEFLIB);
        strcat (buf2, buf);
        fp = fopen (buf2, "r");
        // Copy include filename to filename array.
        strcpy (finame[inclsp + 1], buf2);
        // Reset source line counter.
        srcln[inclsp + 1] = 0;
    }
    return fp;
}

// Open include file.
void
doinclude ()
{
    FILE *inp2;

    blanks ();
    if (inp2 = fix_include_name ()) {
        if (inclsp < INCLSIZ) {
            inclstk[inclsp++] = input2;
            input2 = inp2;
        } else {
            fclose (inp2);
            perror ("too many nested includes");
        }
    } else
        perror ("Could not open include file");
    kill ();
}

// "#asm" pseudo-statement.
// Enters mode where assembly language
// statements are passed intact through
// parser.
void
doasm ()
{
    cmode = 0;
    FOREVER {
        readline ();
        if (match ("#endasm"))
            break;
        if (feof (input))
            break;
        outs (line);
        newline ();
    }
    kill ();
    cmode = 1;
}

void
dodefine ()
{
    addmac ();
}

void
doundef ()
{
    int mp;
    char sname[NAMESIZE];

    if (!symname (sname)) {
        illname ();
        kill ();
        return;
    }

    if (mp = findmac (sname))
        delmac (mp);
    kill ();
}

void
doifdef (int ifdef)
{
    char sname[NAMESIZE];
    int k;

    blanks ();
    ++iflevel;
    if (skiplevel)
        return;
    k = symname (sname) && findmac (sname);
    if (k != ifdef)
        skiplevel = iflevel;
}

void
noiferr ()
{
    perror ("no matching #if...");
}

int
ifline ()
{
    FOREVER {
        readline ();
        if (feof (input))
            return 1;
        if (match ("#ifdef")) {
            doifdef (YES);
            continue;
        } else if (match ("#ifndef")) {
            doifdef (NO);
            continue;
        } else if (match ("#else")) {
            if (iflevel) {
                if (skiplevel == iflevel)
                    skiplevel = 0;
                else if (!skiplevel)
                    skiplevel = iflevel;
            } else
                noiferr ();
            continue;
        } else if (match ("#endif")) {
            if (iflevel) {
                if (skiplevel == iflevel)
                    skiplevel = 0;
                --iflevel;
            } else
                noiferr ();
            continue;
        }
        if (!skiplevel)
            return 0;
    }
}

void
defmac (char *s)
{
    kill ();
    strcpy (line, s);
    addmac ();
}

// Preprocess
// Copies mline to line with special
// treatment of preprocess cmds.
int
cpp ()
{
    int k;
    char c, sname[NAMESIZE];
    int tog;
    // Non-zero if something expanded.
    int cpped;

    cpped = 0;
    // Don't expand lines with
    // preprocessor commands in
    // them.
    if (!cmode || line[0] == '#')
        return 0;

    mptr = lptr = 0;
    while (ch ()) {
        if (ch () == ' '
            || ch () == 9) {
            keepch (' ');
            while (ch () == ' '
                   || ch () == 9)
                gch ();
        } else if (ch () == '"') {
            keepch (ch ());
            gch ();
            while (ch () != '"') {
                if (!ch ()) {
                    perror ("missing quote");
                    return 0;
                }
                if (ch () == '\\')
                    keepch (gch ());
                keepch (gch ());
            }
            gch ();
            keepch ('"');
        } else if (ch () == '\'') {
            keepch ('\'');
            gch ();
            while (ch () != '\'') {
                if (!ch ()) {
                    perror ("missing apostrophe");
                    return 0;
                }
                if (ch () == '\\')
                    keepch (gch ());
                keepch (gch ());
            }
            gch ();
            keepch ('\'');
        } else if (ch () == '/'
                   && nch () == '*') {
            inchar ();
            inchar ();
            while (!(((c = ch ()) == '*') && nch () == '/'))
                if (c == '$') {
                    inchar ();
                    tog = TRUE;
                    if (ch () == '-') {
                        tog = FALSE;
                        inchar ();
                    }
                    if (alpha (c = ch ())) {
                        inchar ();
                        toggle (c, tog);
                    }
                } else {
                    if (!ch ())
                        readline ();
                    else
                        inchar ();
                    if (feof (input))
                        break;
                }
            inchar ();
            inchar ();
        } else if (ch () == '/' & nch () == '/') {
            // one line comment 
            while (gch ());
        } else if (alphanumeric (ch ())) {
            k = 0;
            while (alphanumeric (ch ())) {
                if (k < NAMEMAX)
                    sname[k++] = ch ();
                gch ();
            }
            sname[k] = 0;
            if (k = findmac (sname)) {
                cpped = 1;
                while (c = macq[k++])
                    keepch (c);
            } else {
                k = 0;
                while (c = sname[k++])
                    keepch (c);
            }
        } else
            keepch (gch ());
    }
    keepch (0);
    if (mptr >= MPMAX) {
        perror ("line too long");
        return 9;
    }
    lptr = mptr = 0;
    while (line[lptr++] = mline[mptr++]);
    lptr = 0;
    return cpped;
}

void
preprocess ()
{
    if (ifline ())
        return;
    while (cpp ());
}
