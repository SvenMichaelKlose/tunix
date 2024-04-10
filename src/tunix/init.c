#include <stdio.h>

#define JSR(x) ((void (*) (void)) x) ()

#define INITVIC 0xe5c3

void
main (void)
{
    JSR(INITVIC);

    // Doesn't put it. :(
    puts ("INIT.");
}
