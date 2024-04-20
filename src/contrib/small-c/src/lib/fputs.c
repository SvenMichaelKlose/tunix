#include <stdio.h>

fputs (char *str, FILE *fp)
{
    while (*str)
        fputc (*str++, fp);
}
