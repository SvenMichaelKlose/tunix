#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

char c;
int line   = 0;
int column = 0;

char
in (void)
{
    column++;
    if (feof (stdin))
        return 0;
    return c = fgetc (stdin);
}

void
out (char c)
{
    fputc (c, stdout);
}

void
outs (char * s)
{
    fputs (s, stdout);
}

char
next (void)
{
    c = in ();
    ungetc (c, stdin);
    column--;
    return c;
}

void
error (char * msg)
{
    out (10);
    fflush (stdout);
    fprintf (stderr,
             "On line %d, column %d: %s\n",
             line, column, msg);
    exit (EXIT_FAILURE);
}

char
separator (void)
{
    next ();
    if (c == '"')
        error ("Unexpected double quote further in field.");
    if (c < ' ' || c == ',')
        return in ();
    return 0;
}

void
doquoted (void)
{
    in ();
    while (in ()) {
        if (c == '"') {
            if (next () != '"')
                return;
            in ();
        }
        out (c);
        continue;
    }
    error ("Double quote expected before end of line.");
}

void
docolumn (void)
{
    outs ("<td>");
    if (next () == '"')
        doquoted ();
    else
        while (!separator ())
            out (in ());
    outs ("</td>");
}

void
skipeol (void)
{
    while (isspace (next ()))
        in ();
    column = 0;
}

void
doline (void)
{
    line++;
    outs (" <tr>");
    while (next () >= ' ')
        docolumn ();
    outs ("</tr>\n");
    skipeol ();
}

int
main (void)
{
    outs ("<!doctype html>\n");
    outs ("<html><table>\n");
    while (next ())
        doline ();
    outs ("</table></html>\n");
    return 0;
}
