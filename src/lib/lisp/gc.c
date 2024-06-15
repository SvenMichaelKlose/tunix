#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#define VERBOSE_GC

// Trace and mark reachable objects.
void FASTCALL
mark (lispptr x)
{
    if (!x)
        return;
    if (!MARKED(x)) {
        for (MARK(x); CONSP(x); x = CDR(x)) {
            mark (x);
            mark (CAR(x));
        }
        if (x && SYMBOLP(x))
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
size_t gap;
#ifdef __CC65__
#pragma zpsym ("sweep_completed")
#pragma zpsym ("s")
#pragma zpsym ("d")
#pragma zpsym ("n")
#pragma zpsym ("p")
#pragma zpsym ("r")
#pragma zpsym ("xlat")
#pragma zpsym ("minxlat")
#pragma zpsym ("gap")
#pragma bss-name (pop)
#endif

// Copy marked objects over deleted ones and make a
// relocation table containing the addresses and sizes of
// the deleted objects.
void
sweep ()
{
    xlat  = heap_end;
    minxlat = heap_free + sizeof (lispptr) * 2;
    while (*s) {
        n = objsize (s);
        if (MARKED(s)) {
            // Copy object with mark bit cleared.
            *d++ = *s++ & ~TYPE_MARKED;
            while (--n)
                *d++ = *s++;
        } else {
            if (last_sweeped == d) {
                // Append gap to previous one.
                *(unsigned *) xlat += n;
            } else {
                // Make new entry in relocation table.
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

// Sum up gap sizes in relocation table up to the pointer
// and subtract that from the pointer.
lispptr FASTCALL
relocate_ptr (char * x)
{
    gap = 0;
    for (r = heap_end; r != xlat;) {
        r -= sizeof (lispptr);
        if (*(char **) r > x)
            break;
        r -= sizeof (unsigned);
        gap += *(unsigned *) r;
    }
    return x - gap;
}

// Relocate pointers on heap and stack.
void
relocate (void)
{
#ifdef VERBOSE_GC
    out ('R');
#endif
    universe = relocate_ptr (universe);
    for (p = heap_start; *p; p += objsize (p)) {
        if (p == s)
            p = d; // Jump over sweep gap.

        if (CONSP(p)) {
            SETCAR(p, relocate_ptr (CAR(p)));
            SETCDR(p, relocate_ptr (CDR(p)));
        } else if (SYMBOLP(p))
            SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
    }
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        *(lispptr *)p = relocate_ptr (*(lispptr *) p);
}

void
gc (void)
{
#ifdef VERBOSE_GC
    char * tmp = heap_free;
    out ('M');
#endif

    // Trace objects.
    mark (universe);
    mark (return_sym);
    mark (return_name);
    mark (return_value);
    mark (go_sym);
    mark (go_tag);
    mark (delayed_eval);
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        mark (*(lispptr *) p);

    // Remove and relocate.
    last_sweeped = NULL;
    sweep_completed = false;
    s = d = heap_start;  // Relocation source + dest.
    do {
#ifdef VERBOSE_GC
        out ('S');
#endif
        sweep ();
        relocate ();
    } while (!sweep_completed);

#ifdef VERBOSE_GC
    outs (": ");
    outnu ((unsigned) (tmp - heap_free));
    outs (" bytes freed.");
    terpri ();
#endif
}
