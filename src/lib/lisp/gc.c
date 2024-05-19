#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

// Trace objects and mark them.
void FASTCALL
mark (lispptr x)
{
    if (!MARKED(x)) {
        MARK(x);
        if (CONSP(x)) {
            while (CONSP(x)) {
                mark (CAR(x));
                x = CDR(x);
                MARK(x);
            }
        } else if (SYMBOLP(x))
            mark (SYMBOL_VALUE(x));
    }
}

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
bool   sweep_completed;
char * s;   // Source
char * d;   // Destination
unsigned n;
char * p;
char * r;
char * xlat;
char * last_sweeped;
char * minxlat;
size_t rel;
#ifdef __CC65__
#pragma zpsym ("sweep_completed")
#pragma zpsym ("s")
#pragma zpsym ("d")
#pragma zpsym ("n")
#pragma zpsym ("p")
#pragma zpsym ("r")
#pragma zpsym ("xlat")
#pragma zpsym ("minxlat")
#pragma zpsym ("rel")
#pragma bss-name (pop)
#endif

// Copy marked objects over deleted ones.
// Make list of addresses of deleted objects and their size
// for the pointer relocation pass.
void
sweep ()
{
    xlat  = heap_end;
    minxlat = heap_free + sizeof (lispptr) * 2;
    while (*s) {
        n = objsize (s);
        if (MARKED(s)) {
            *d++ = *s++ & ~TYPE_MARKED;
            while (--n)
                *d++ = *s++;
        } else {
            if (last_sweeped == d) {
                *(unsigned *) xlat += n;
            } else {
                last_sweeped = d;
                // Log gap position and size.
                xlat -= sizeof (lispptr);
                *(lispptr *) xlat = s;
                xlat -= sizeof (unsigned);
                *(unsigned *) xlat = n;

                // Interrupt sweep if xlat table is full.
                if (xlat <= minxlat)
                    return;
            }

            s += n;
        }
    }
    *d = 0;
    heap_free = d;
    sweep_completed = true;
}

lispptr FASTCALL
relocate_ptr (char * x)
{
    rel = 0;
    for (r = heap_end; r != xlat;) {
        r -= sizeof (lispptr);
        if (*(char **) r > x)
            break;
        r -= sizeof (unsigned);
        rel += *(unsigned *) r;
    }
    return x - rel;
}

void
relocate (void)
{
    for (p = heap_start; *p; p += objsize (p)) {
        if (p == s)
            p = d; // Jump over sweep gap.

        if (CONSP(p)) {
            RPLACA(relocate_ptr (CAR(p)), p);
            RPLACD(relocate_ptr (CDR(p)), p);
        } else if (SYMBOLP(p))
            SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
    }
}

void
gc (void)
{
    // Trace objects.
    mark (universe);
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        mark (*(lispptr *) p);

    // Remove and relocate.
    last_sweeped = NULL;
    sweep_completed = false;
    s = d = heap_start;  // Relocation source + dest.
    do {
        sweep ();
        relocate ();
    } while (!sweep_completed);
}
