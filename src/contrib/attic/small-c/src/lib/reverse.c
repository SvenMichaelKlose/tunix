#include <stdio.h>

// Reverse a character string.
// Reference CPL p 59 
reverse (char *s)
{
    int i, j;
    char c;
    i = 0;
    j = strlen (s) - 1;
    while (i < j) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
        i++;
        j--;
    }
    return s;
}
