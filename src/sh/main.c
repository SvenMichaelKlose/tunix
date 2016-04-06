#define MAX_LINE_LENGTH 255

#include <stdio.h>

#define CHAR_RETURN     13
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
main (char ** argv, int argc)
{ 
    char line[MAX_LINE_LENGTH];
    int count;

    for (count = 0; count < 100; count++)
        printf ("testtestsdfasfdasfsfsfdsfsdfsf!!!!!!!!!\n");

    while (1) {
        count = get_line (stdout, stdin, line);
    }

    return 0;
}
