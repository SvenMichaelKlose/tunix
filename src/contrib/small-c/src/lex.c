#include <stdio.h>
#include <error.h>

#include "defs.h"
#include "data.h"
#include "io.h"
#include "preproc.h"
#include "gen.h"
#include "ir.h"
#include "lex.h"

char
alpha (char c)
{
    c = c & 127;
    return c >= 'a' && c <= 'z'
           || c >= 'A' && c <= 'Z'
           || c == '_';
}

char
numeric (char c)
{
    c = c & 127;
    return c >= '0' && c <= '9';
}

char
alphanumeric (char c)
{
    return alpha (c) || numeric (c);
}

void
need_semicolon ()
{
    if (!match (";"))
        perror ("missing semicolon");
}

void
junk ()
{
    if (alphanumeric (inbyte ()))
        while (alphanumeric (ch ()))
            gch ();
    else
        while (alphanumeric (ch ())) {
            if (!ch ())
                break;
            gch ();
        }
    blanks ();
}

int
endst ()
{
    blanks ();
    return streq (line + lptr, ";")
           || !ch ();
}

void
needbrack (char *str)
{
    if (match (str))
        return;
    perror ("missing bracket");
    gen_comment ();
    outs (str);
    newline ();
}

int
sstreq (char *str1)
{
    return streq (line + lptr, str1);
}

// Indicates whether or not the current
// substring in the source line matches
// a literal string.  Accepts the
// address of the current character in
// the source line and the address of
// the a literal string, and returns the
// substring length if a match occurs
// and zero otherwise.
int
streq (char *str1, char *str2)
{
    int k;

    k = 0;
    while (str2[k]) {
        if (str1[k] != str2[k])
            return 0;
        k++;
    }
    return k;
}

// Compare zero-terminated string
int
astreq (char str1[], char str2[],
        int len)
{
    int k;
    k = 0;
    while (k < len) {
        if (str1[k] != str2[k])
            break;
        if (!str1[k])
            break;
        if (!str2[k])
            break;
        k++;
    }
    if (alphanumeric (str1[k]))
        return 0;
    if (alphanumeric (str2[k]))
        return 0;
    return k;
}

// Looks for a match between a literal
// string and the current token in the
// input line.  It skips over the token
// and returns true if a match occurs
// otherwise it retains the current
// position in the input line and
// returns false there is no
// verification that all of the token
// was matched.
int
match (char *lit)
{
    int k;
    blanks ();
    if (k = streq (line + lptr, lit)) {
        lptr = lptr + k;
        return 1;
    }
    return 0;
}

// Compares zero-terminated strings.
// Advances line pointer only if match
// found.  It assumes that an
// alphanumeric (including underscore)
// comparison is being made and
// guarantees that all of the token in
// the source line is scanned in the
// process.
int
amatch (char *lit, int len)
{
    int k;

    blanks ();
    if (k = astreq (line + lptr, lit, len)) {
        lptr = lptr + k;
        while (alphanumeric (ch ()))
            inbyte ();
        return 1;
    }
    return 0;
}

void
blanks ()
{
    FOREVER {
        while (!ch ()) {
            preprocess ();
            if (feof (input))
                break;
        }
        if (ch () == ' ')
            gch ();
        else if (ch () == 9)
            gch ();
        else
            return;
    }
}

// Returns one of declaration types
// CCHAR, CINT, UCHAR or UINT.
int
get_type ()
{
    if (amatch ("register", 8)) {
        if (amatch ("char", 4))
            return CCHAR;
        if (amatch ("int", 3))
            return CINT;
        return CINT;
    }
    if (amatch ("unsigned", 8)) {
        if (amatch ("char", 4))
            return UCHAR;
        if (amatch ("int", 3))
            return UINT;
        return CINT;
    } else if (amatch ("signed", 8)) {
        if (amatch ("char", 4))
            return CCHAR;
        if (amatch ("int", 3))
            return CINT;
        return CINT;
    }
    if (amatch ("char", 4))
        return CCHAR;
    if (amatch ("int", 3))
        return CINT;
    if (amatch ("struct", 6))
        return STRUCT;
    return 0;
}
