#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

// Trace and mark reachable objects.
void FASTCALL
mark (lispptr x)
{
    if (x && !MARKED(x)) {
        MARK(x);
        for (; _CONSP(x); x = CDR(x)) {
            MARK(x);
            mark (CAR(x));
        }
        if (x) {
            MARK(x);
            if (_SYMBOLP(x))
                mark (SYMBOL_VALUE(x));
        }
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

lispptr last_kept_sym;

// Copy marked objects over deleted ones and make a
// relocation table containing the addresses and sizes of
// the deleted objects.
void
sweep ()
{
    xlat = heap_end;
    while (*s) {
#ifndef NDEBUG
        if (s >= heap_end)
            internal_error ("Sweep overflow");
#endif
        n = objsize (s);
        if (MARKED(s)) {
            if (_NAMEDP(s) && SYMBOL_LENGTH(s)) {
                SYMBOL_NEXT(last_kept_sym) = s;
                last_kept_sym = d;
            }

#ifdef SKIPPING_SWEEP
            if (s == d) {
                *d &= ~TYPE_MARKED;
                s += n;
                d = s;
            } else {
#endif
                // Copy object with mark bit cleared.
                *d++ = *s++ & ~TYPE_MARKED;
                while (--n)
                    *d++ = *s++;
#ifdef SKIPPING_SWEEP
            }
#endif
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

#ifndef NDEBUG
                if (xlat <= heap_free)
                    internal_error ("No reloc.");
#endif
            }

            s += n;
        }
    }
    *d = 0;
    heap_free = d;
    sweep_completed = true;

    SYMBOL_NEXT(last_kept_sym) = nil;
    last_symbol = last_kept_sym;
}

// Sum up gap sizes in relocation table up to the pointer
// and subtract it from the pointer.
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

    // Relocate elements on heap.
    for (p = heap_start; *p; p += objsize (p)) {
#ifndef NDEBUG
        if (p >= heap_end)
            internal_error ("Heap reloc overflow");
#endif
        //if (p == s)
            //p = d; // Jump over sweep gap.

        if (_CONSP(p)) {
            SETCAR(p, relocate_ptr (CAR(p)));
            SETCDR(p, relocate_ptr (CDR(p)));
        } else if (_SYMBOLP(p))
            SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
        if (_NAMEDP(p) && SYMBOL_LENGTH(p))
            SET_SYMBOL_NEXT(p, relocate_ptr (SYMBOL_NEXT(p)));
    }

    // Relocate GC stack elements.
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

    // Trace root objects.
    mark (universe);
    mark (return_sym);
    mark (return_name);
    mark (return_value);
    mark (go_sym);
    mark (go_tag);
    mark (delayed_eval);

    // Trace object stack.
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        mark (*(lispptr *) p);

    // Remove and relocate.
    last_sweeped = NULL;
    sweep_completed = false;
    s = d = heap_start;  // Relocation source + dest.
    last_kept_sym = first_symbol;
    //do {
#ifdef VERBOSE_GC
        out ('S');
#endif
        sweep ();
        relocate ();
    //} while (!sweep_completed);

#ifdef VERBOSE_GC
    outs (": ");
    outn (tmp - heap_free);
    outs ("B freed.");
    terpri ();
#endif
}
