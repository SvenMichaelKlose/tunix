#include <conio.h>
#include <stdio.h>
#include <errno.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC 0xe5c3

void
main (void)
{
    *(char *) 0x900f = 0x1b;
    clrscr ();
    printf ("Welcome to TUNIX!\n");
    printf ("THIS LINE WON'T PRINT!\n");
    while (1);
}
