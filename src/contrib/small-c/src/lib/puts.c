#include <stdio.h>

#define EOL 10

puts (char *str)
{
    while (*str)
        putchar (*str++);
    putchar (EOL);
}
