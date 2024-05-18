#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

void
unmark (void)
{
    char * p;
    for (p = heap_start; UNMARK(p); p += objsize (p));
}

// Trace objects and mark them.
void
mark (lispptr x)
{
    if (!MARKED(x)) {
        MARK(x);
        if (CONSP(x)) {
            // Loop over list.
            while (CONSP(x)) {
                mark (CAR(x));
                x = CDR(x);
                MARK(x);
            }
        } else if (SYMBOLP(x))
            mark (SYMBOL_VALUE(x));
    }
}

bool   sweep_completed;
char * s;   // Source
char * d;   // Destination
char * xlat;
char * minxlat;

// Copy marked objects over deleted ones.
// Make list of addresses of deleted objects and their size
// for the pointer relocation pass.
void
sweep ()
{
    unsigned c;
    //char * l;

    xlat  = heap_end;
    minxlat = heap_free + sizeof (lispptr) * 2;
    while (*s) {
        if (s >= heap_end) {
            errouts ("Sweep accross heap end.");
            while (1);
        }
        c = objsize (s);
        if (!MARKED(s)) {
            // Get address of previous relocation info.
/*
            l = xlat + sizeof (lispptr) + sizeof (unsigned);

            // Enlarge previous gap.
            if (xlat != heap_end && l == s)
                *(unsigned *) (xlat + 1) += c;
            else {
*/
                // Log gap position and size.
                xlat -= sizeof (lispptr);
                *(lispptr *) xlat = s;
                xlat -= sizeof (unsigned);
                *(unsigned *) xlat = c;

                // Interrupt sweep if xlat table is full.
                if (xlat <= minxlat)
                    return;
            //}
            s += c;
        } else
            while (c--)
                *d++ = *s++;
    }
    *d = 0;
    heap_free = d;
    sweep_completed = true;
    errouts ("Sweep complete.\n\r");
}

// Sum up number of bytes freed before address to relocate
// and subtract it.
lispptr
relocate_ptr (char * x)
{
    char * p;
    char * l;
    size_t rel = 0;
    for (p = heap_end; p != xlat;) {
        p -= sizeof (char *);
        l = *(char **) p;
        if (l > x)
            break;
        p -= sizeof (unsigned);
        rel += *(unsigned *) p;
    }
    return x - rel;
}

void
relocate (void)
{
    char * p;
    for (p = heap_start; *p; p += objsize (p)) {
        if (p == s)
            p = d; // Jump over copy gap.

        if (CONSP(p)) {
            RPLACA(relocate_ptr (CAR(p)), p);
            RPLACD(relocate_ptr (CDR(p)), p);
        } else if (SYMBOLP(p))
            SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
    }
    errouts ("Relocation complete.\n\r");
}

void
gc (void)
{
    // Remove flag from all objects.
    unmark ();

    // Trace objects.
    mark (universe);

    // Remove and relocate.
    sweep_completed = false;
    s = d = heap_start;  // Relocation source + dest.
    do {
        errouts ("sweep\n\r");
        sweep ();
        errouts ("relocate\n\r");
        relocate ();
    } while (!sweep_completed);
}
