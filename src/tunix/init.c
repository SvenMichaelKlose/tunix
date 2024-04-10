#include <stdio.h>

void
main (void)
{
    // Init VIC.
    ((void (*) (void)) 0xe5c3) ();

    // Doesn't put it. :(
    puts ("INIT.");
}
