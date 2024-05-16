#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include "liblisp.h"

// Trace objects and mark them.
void
mark (lispptr x)
{
    if (MARKED(x)) {
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
    uchar  c;
    lispptr * l;
    s = d = (char *) heap_start;
    xlat  = heap_end;
    while (*s) {
        c = OBJSIZE(s);
        if (!MARKED(s)) {
            l = (lispptr *) (xlat + sizeof (lispptr) + sizeof (uchar));
            // Enlarge previous gap.
            if (xlat != heap_end && ((char *) *l) + c == s)
                *xlat += c;
            else {
                l = (lispptr *) xlat;
                // Log gap position and size.
                *--l = (lispptr) s;
                xlat = (char *) l;
                *--xlat = c;

                // Interrupt sweep if xlat table is full.
                if (xlat <= minxlat)
                    return;
            }
            s += c;
        } else
            while (c--)
                *d++ = *s++;
    }
    *d = 0;
    heap_free = d;
    sweep_completed = true;
}

// Sum up number of bytes freed before address to relocate
// and subtract it.
lispptr
relocate_ptr (lispptr x)
{
    char * p;
    lispptr * l;
    size_t rel = 0;
    for (p = heap_end; p != xlat;) {
        l = (lispptr *) p;
        if (*--l > x)
            return (char *) x - rel;
        p = (char *) l;
        rel += *--p;
    }
    return (char *) x - rel;
}

void
relocate (void)
{
    char * p;
    for (p = heap_start; *p; p += OBJSIZE(p)) {
        if (p == s)
            p = d;
        if (SYMBOLP(p))
            SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
        else if (CONSP(p)) {
            RPLACA(relocate_ptr (CAR(p)), p);
            RPLACD(relocate_ptr (CDR(p)), p);
        }
    }
}

void
gc ()
{
    // Calculate lowest address of xlat table.
    minxlat = heap_free + sizeof (char *) + sizeof (char);

    mark (universe);
    sweep_completed = false;
    s = d = heap_start;  // Relocation pointers.
    while (!sweep_completed) {
        sweep ();
        relocate ();
    }
}
