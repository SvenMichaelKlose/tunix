#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <term/libterm.h>

// Left here for reuse.
//#pragma bss-name (push, "ZEROPAGE")
//char *    heap;
//#pragma zpsym ("heap");
//#pragma bss-name (pop)

extern void * _PROC_LOAD__;
extern void * _PROC_RUN__;
extern size_t _PROC_SIZE__;

int
main (int argc, char * argv[])
{
    (void) argc;
    (void) argv;

    term_init ();
    term_puts ("TUNIX\n\r");

    memcpy (_PROC_RUN__, _PROC_LOAD__, _PROC_SIZE__);

    while (1);
    return 0;
}
