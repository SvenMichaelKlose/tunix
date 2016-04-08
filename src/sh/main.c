#define MAX_LINE_LENGTH 255
#define MAX_PARAMS 16

#include <stdio.h>
#include <stdlib.h>

#define CHAR_RETURN     10
#define CHAR_BACKSPACE  8

void
print_prompt (FILE * out)
{
    fprintf (out, "$ ");
}

int
get_line (FILE * out, FILE * in, char * line)
{
    int count = 0;
    char c;

    print_prompt (out);

    while (1) {
        c = fgetc (in);
        if (c == CHAR_RETURN) {
            line[count++] = 0;
            fputc (10, out);
            return count;
        }
        if (c == CHAR_BACKSPACE) {
            if (count) {
                line[--count] = 0;
                fputc (c, out);
            }
            continue;
        }
        if (count == MAX_LINE_LENGTH)
            continue;
        line[count++] = c;
        fputc (c, out);
    }
}

int
parse (char ** values, char * in)
{
    char * tmp;
    char * out;
    size_t len;

    *values = NULL;
    while (*in == ' ')
        in++;
    if (!*in)
        return 0;
    tmp = in;
    len = 0;
    while (*tmp++ > ' ')
        len++;
    *values = out = malloc (len + 1);
    while (*in > ' ')
        *out++ = *in++;
    *out = 0;
    return 1 + parse (++values, in);
}

int
main (char ** argv, int argc)
{ 
    char * line = malloc (MAX_LINE_LENGTH + 1);
    char * values[MAX_PARAMS];
    int count;

    while (1) {
        get_line (stdout, stdin, line);
        count = parse (values, line);
        printf ("%d\n", count);
        for (count = 0;; count++) {
            if (!values[count])
                break;
            printf ("%s\n", values[count]);
            free (values[count]);
        }
    }

    return 0;
}
