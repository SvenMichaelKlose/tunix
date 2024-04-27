#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "token-table.c"

FILE * in;
FILE * out;
char buffer[256];

char
is_keyword (char len)
{
    unsigned char i, l;
    int ntok = sizeof (tokens) / sizeof (struct token);

    for (i = 0; i < ntok; i++) {
        l = strlen (tokens[i].match);
        if (l == len && memcmp (tokens[i].match, buffer, len) == 0)
            return i + 1;
    }
    return 0;
}

void
emit (char type, char i)
{
    unsigned char j;

    fputc (type, out);
    fputc (i, out);
    for (j = 0; j < i; j++)
        fputc (buffer[j], out);
}

void
tokenize ()
{
    char c, k;
    unsigned char i = 0;

    while (!feof (in)) {
        c = fgetc (in);
        if (!i) {
            if (c == '"') {
                while (1) {
                    if (feof (in)) {
                        exit (-1);
                    }
                    c = fgetc (in);
                    if (c == '"')
                        break;
                    buffer[i++] = c;
                }
                emit ('s', i);
                i = 0;
                continue;
            }
            if (c == '\'') {
                fputc ('c', out);
                fputc (fgetc (in), out);
                fgetc (in);
                i = 0;
                continue;
            }
        }
        if (isalpha ((unsigned char) c) || c == '_') {
            buffer[i++] = c;
            continue;
        }
        if (i) {
            buffer[i] = 0;
            if (k = is_keyword (i))
                fputc ('0' + k - 1, out);
            else
                emit ('i', i);
            i = 0;
            ungetc (c, in);
            continue;
        }
        fputc ('x', out);
        fputc (c, out);
    }
}

int
main (int argc, char *argv[])
{
    if (argc != 3) {
        fprintf (stderr, "Usage: %s <infile> <outfile>\n", argv[0]);
        return 1;
    }
    in = fopen (argv[1], "r");
    if (in == NULL) {
        fprintf (stderr, "Error opening file %s\n", argv[1]);
        return 1;
    }
    out = fopen (argv[2], "w");
    if (out == NULL) {
        fprintf (stderr, "Error opening output file.\n");
        return 1;
    }
    tokenize ();
    fclose (in);
    fclose (out);
    return 0;
}
