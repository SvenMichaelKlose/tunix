#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

const char *keywords[] = {
    "int", "return", "void", "if",
    "else", "while", "for", "break",
    "continue", "char", "do", "switch",
    "default", "case", "enum",
    "struct", "typedef", "union",
    "const", "sizeof", "float",
    "double", "short", "long",
    "unsigned", "signed", "static",
    "volatile", "goto", "register",
    "extern", "auto", NULL
};

FILE * in;
FILE * out;
char buffer[256];

char
is_keyword (char len)
{
    unsigned char i, l;

    for (i = 0; keywords[i] != NULL; i++) {
        l = strlen (keywords[i]);
        if (l == len && memcmp (keywords[i], buffer, len) == 0)
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
        //putchar (c);
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
            fprintf (stderr, "%s\n", buffer);
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
