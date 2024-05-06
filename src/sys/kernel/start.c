#include <stdio.h>

void tunix (void);

extern void debug (void);
void debug () {};

int
main (char argc, char * argv[])
{
    (void) argc;
    (void) argv;

    printf ("Starting TUNIX.\n"),
    debug ();
    tunix ();
}
