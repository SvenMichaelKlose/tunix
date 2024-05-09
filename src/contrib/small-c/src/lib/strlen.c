#include <stdio.h>

strlen (char *s)
{
    int i;
    i = 0;
    while (*s++)
        i++;
    return i;
}
