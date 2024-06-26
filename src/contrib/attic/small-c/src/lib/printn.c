// Print a number in any radix.

#include <stdio.h>

#define DIGARR "0123456789ABCDEF"

printn (int number, int radix, FILE *file)
{
    int i;
    char *digitreps;
    if (number < 0 & radix == 10) {
        fputc ('-', file);
        number = -number;
    }
    if ((i = number / radix) != 0)
        printn (i, radix, file);
    digitreps = DIGARR;
    fputc (digitreps[number % radix], file);
}
