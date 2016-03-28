#include <stdio.h>

int
main (char ** argv, int argc)
{ 
    char c;

    printf ("$ ");

    while (1) {
        c = fgetc (stdin);
        if (c)
            putchar (c);
    }

    return 0;
}
