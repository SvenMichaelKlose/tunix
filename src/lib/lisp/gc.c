#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef DUMP_SWEEP
#include <stdio.h>
#endif

// Trace and mark reachable objects.
void FASTCALL
mark (lispptr x)
{
    CHKPTR(x);
    if (x && !MARKED(x)) {
        MARK(x);
        for (; _CONSP(x); x = CDR(x)) {
            CHKPTR(x);
            MARK(x);
            mark (CAR(x));
        }
        CHKPTR(x);
        if (x) {
            MARK(x);
            if (_SYMBOLP(x))
                mark (SYMBOL_VALUE(x));
        }
    }
}

extern char * xlat_start;
extern char * xlat_end;
char * xlat_start;
char * xlat_end;
lispptr last_kept_sym;

#ifdef FRAGMENTED_HEAP
extern struct heap_fragment * heap;
extern struct heap_fragment   heaps[];
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
unsigned n;

char * s;   // Source
char * d;   // Destination

// Separate pointers for relocation to keep s & d intact
// when a garbage collection has to be continued.  That
// feature has been removed temporarily because.... forgot.
char * p;
char * r;

char * xlat;
char * last_sweeped; // For merging consecutive gaps.
size_t gapsize;
#ifdef __CC65__
#pragma zpsym ("s")
#pragma zpsym ("d")
#pragma zpsym ("n")
#pragma zpsym ("p")
#pragma zpsym ("r")
#pragma zpsym ("xlat")
#pragma zpsym ("gapsize")
#pragma bss-name (pop)
#endif

// Copy marked objects over deleted ones and make a
// relocation table containing the addresses and sizes of
// the deleted objects.
void
sweep ()
{
    // Start with first heap.
#ifdef FRAGMENTED_HEAP
    struct heap_fragment * heap = heaps;
#endif

#ifdef DUMP_SWEEP
    size_t total_removed = 0;
#endif

    // Invalidate pointer to last sweeped object.
    last_sweeped = nil;

    // Get start of symbol list.
    last_kept_sym = first_symbol;

    // Get start of relocation table.
    xlat = xlat_end;

    // Get heap pointers.
#ifdef FRAGMENTED_HEAP
    do {
        heap_start = heap->start;
        heap_free = heap->free;
#ifndef NDEBUG
        heap_end = heap->end;
#endif
#endif // #ifdef FRAGMENTED_HEAP
#ifdef VERBOSE_GC
    out ('S');
#endif

        // Sweep heap.
        s = d = heap_start;
        CHKPTR(s);
        while (*s) {
#ifdef DUMP_SWEEP
            dump_lispptr (s);
#endif
#ifndef NDEBUG
            if (s >= heap_end)
                internal_error ("Sweep overflow");
#endif
            n = objsize (s);
            if (MARKED(s)) {
                // Link this and last named symbol.
                if (_NAMEDP(s) && SYMBOL_LENGTH(s)) {
                    SYMBOL_NEXT(last_kept_sym) = s;
                    last_kept_sym = d;
                }

#ifdef SKIPPING_SWEEP
                // Clear mark bit.
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
                    // Enlarge immediate previous gap.
                    *(unsigned *) xlat += n;
#ifdef DUMP_SWEEP
                    total_removed += n;
                    printf ("Enlarged gap to %dB. Total removed: %dB\n", *(unsigned *) xlat, total_removed);
#endif
                } else {
                    // Memorize this latest gap.
                    last_sweeped = d;

                    // Log gap position and size.
                    xlat -= sizeof (lispptr);
                    *(lispptr *) xlat = s;
                    xlat -= sizeof (unsigned);
                    *(unsigned *) xlat = n;
#ifdef DUMP_SWEEP
                    total_removed += n;
                    printf ("Created gap of %dB. Total removed: %dB\n", n, total_removed);
#endif

#ifndef NDEBUG
                    if (xlat == xlat_start)
                        internal_error ("Relocation table overflow.");
#endif
                }

                s += n;
                CHKPTR(s);
            }
        }

        // Mark end of heap.
        *d = 0;

        // Save free pointer.
#ifdef FRAGMENTED_HEAP
        heap->free = d;

        // Undo address shifting with negative gap entry in
        // order to not affect the following heap's pointers.
        xlat -= sizeof (lispptr);
        *(lispptr *) xlat = (lispptr) (heap_free - d);
        xlat -= sizeof (unsigned);
        *(unsigned *) xlat = 0;

#ifndef NDEBUG
        if (xlat == xlat_start)
            internal_error ("Relocation table overflow.");
#endif
    } while ((++heap)->start);
#else // #ifdef FRAGMENTED_HEAP
    // Save free pointer.
    heap_free = d;
#endif

    // End symbol list.
    SYMBOL_NEXT(last_kept_sym) = nil;

    // Save last symbol for next allocation with name.
    last_symbol = last_kept_sym;
}

// Sum up gap sizes in relocation table up to the pointer
// and subtract it from the pointer.
lispptr FASTCALL
relocate_ptr (char * x)
{
    gapsize = 0;
    for (r = xlat_end; r != xlat;) {
        r -= sizeof (lispptr);
        if (*(char **) r > x)
            break;
        r -= sizeof (unsigned);
        gapsize += *(unsigned *) r;
    }
    return x - gapsize;
}

// Relocate pointers on heap, stack, and in global vars.
void
relocate (void)
{
    // Relocate global variables.
    universe         = relocate_ptr (universe);
    return_name      = relocate_ptr (return_name);
    return_value     = relocate_ptr (return_value);
    go_tag           = relocate_ptr (go_tag);
#ifndef NO_DEBUGGER
    current_expr     = relocate_ptr (current_expr);
    current_toplevel = relocate_ptr (current_toplevel);
#endif

#ifdef FRAGMENTED_HEAP
    heap = heaps;
    do {
        heap_start = heap->start;
#endif
#ifdef VERBOSE_GC
    out ('R');
#endif

        // Relocate elements on heap.
        for (p = heap_start; *p; p += objsize (p)) {
            CHKPTR(p);
#ifndef NDEBUG
            if (p >= heap_end)
                internal_error ("Heap reloc overflow");
#endif
            if (_CONSP(p)) {
                SETCAR(p, relocate_ptr (CAR(p)));
                SETCDR(p, relocate_ptr (CDR(p)));
            } else if (_SYMBOLP(p))
                SET_SYMBOL_VALUE(p, relocate_ptr (SYMBOL_VALUE(p)));
            if (_NAMEDP(p) && SYMBOL_LENGTH(p))
                SET_SYMBOL_NEXT(p, relocate_ptr (SYMBOL_NEXT(p)));
        }
#ifdef FRAGMENTED_HEAP
    } while ((++heap)->start);
#endif

    // Relocate GC stack.
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        *(lispptr *)p = relocate_ptr (*(lispptr *) p);
}

#ifdef FRAGMENTED_HEAP
void
switch_heap ()
{
    heap_start = heap->start;
    heap_free = heap->free;
    heap_end = heap->end;
    heap++;
}
#endif

void
gc (void)
{
#ifdef VERBOSE_GC
    char * tmp;
#endif

    // Switch to next heap instead.
#ifdef FRAGMENTED_HEAP
    if (heap->start) {
#ifdef VERBOSE_GC
        out ('N');
#endif
        goto next_heap;
    }
#endif

    // No more heaps.  Switch to first.
#ifdef FRAGMENTED_HEAP
    heap = heaps;
    switch_heap ();
#endif

    // This won't work.  Sum with sweeps.
#ifdef VERBOSE_GC
    tmp = heap_free;
    out ('M');
#endif

    // Mark universe.
    mark (universe);
    mark (return_sym);
    mark (return_name);
    mark (return_value);
    mark (go_sym);
    mark (go_tag);
    mark (delayed_eval);

    // Mark object stack.
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        mark (*(lispptr *) p);

    sweep ();
    relocate ();

    // Start over with first heap.
#ifdef FRAGMENTED_HEAP
    heap = heaps;
next_heap:
    switch_heap ();
#endif

#ifdef VERBOSE_GC
    outs (": ");
    outn (tmp - heap_free);
    outs ("B freed.");
    terpri ();
#endif
}
