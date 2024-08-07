#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef DUMP_SWEEPED
#include <stdio.h>
#endif

// Relocation table
extern char * xlat_start;
extern char * xlat_end;
char * xlat_start;
char * xlat_end;

extern lispptr lisp_fnin;
extern lispptr lisp_fnout;

lispptr * global_pointers[] = {
    &universe, &t, &delayed_eval,
    &block_sym,
    &quote, &quasiquote, &unquote, &unquote_spliced,
    &return_sym, &return_name, &return_value,
    &go_sym, &go_tag,
    &lisp_fnin, &lisp_fnout,
    &current_expr,
#ifndef NAIVE
    &current_toplevel,
#endif
    &current_function,
    &unexpanded_toplevel,
    &unevaluated_arg1,
#ifndef NO_DEBUGGER
    &onerror_sym,
    &debug_step,
#endif
#ifndef NO_MACROEXPAND
    &macroexpand_sym,
#endif
    &highlighted,

    // To be safe:
    &x, &args, &argdefs, &arg1, &arg2, &arg2c,
    &list_start, &list_last,
    &value, &va,
    NULL
};

lispptr ** gp;

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

// End of singly-linked list of named symbols.
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
bool   xlat_full;
char * last_sweeped; // For merging consecutive gaps.
size_t gapsize;
#ifdef __CC65__
#pragma zpsym ("s")
#pragma zpsym ("d")
#pragma zpsym ("n")
#pragma zpsym ("p")
#pragma zpsym ("r")
#pragma zpsym ("xlat")
#pragma zpsym ("xlat_full")
#pragma zpsym ("gapsize")
#pragma bss-name (pop)
#endif

#ifdef FRAGMENTED_HEAP
size_t total_removed;
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

    // Invalidate pointer to last sweeped object.
    // Required to merge gaps.
    last_sweeped = nil;

    // Get start of singly-linked list of named symbols.
    last_kept_sym = universe;

    // Initialize relocation table.
    xlat = xlat_end;    // Point to its start.
    xlat_full = false;  // Mark it as not full.

#if defined(DUMP_MARKED) || defined(DUMP_SWEEPED)
    outs ("Sweep objects:"); terpri ();
#endif

    // Get heap pointers.
#ifdef FRAGMENTED_HEAP
    do {
        total_removed = 0;
        heap_start = heap->start;
        heap_free = heap->free;
#ifndef NDEBUG
        heap_end = heap->end;
#endif
#endif // #ifdef FRAGMENTED_HEAP
#ifdef VERBOSE_GC
    out ('S');
#endif

        // Sweep one heap.
        s = d = heap_start;
        while (*s) {
#ifdef PARANOID
            if (s >= heap_end)
                internal_error ("Sweep overflow");
#endif

            // Get size of object.
            n = objsize (s);

            // Keep/copy marked object.
            if (MARKED(s) || xlat_full) {
#ifdef DUMP_MARKED
                dump_lispptr (s);
#endif
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
#ifdef DUMP_SWEEPED
                dump_lispptr (s);
#endif
                // Remove object.
                if (last_sweeped == d) {
                    // Merge with previous gap.
                    *(unsigned *) xlat += n;
#ifdef FRAGMENTED_HEAP
                    total_removed += n;
#endif
                } else {
                    // Memorize this latest gap.
                    last_sweeped = d;

                    // Log gap position and size.
                    xlat -= sizeof (lispptr);
                    *(lispptr *) xlat = s;
                    xlat -= sizeof (unsigned);
                    *(unsigned *) xlat = n;
#ifdef FRAGMENTED_HEAP
                    total_removed += n;
#endif
                }

                // Step to next object.
                s += n;

                // Flag relocation table being full and
                // the rest of the objects won't be sweeped.
                if (xlat == xlat_start)
                    xlat_full = true;
            }
        }

        // Mark end of heap.
        *d = 0;

#ifdef FRAGMENTED_HEAP
        // Save free pointer.
        heap->free = d;

        // Undo address shifting with negative gap entry in
        // order to not affect the following heap's pointers.
        xlat -= sizeof (lispptr);
        *(lispptr *) xlat = s;
        xlat -= sizeof (unsigned);
        *(unsigned *) xlat = -total_removed;
#ifdef VERBOSE_GC
        outn (total_removed); outs ("B freed."); terpri ();
#endif
    } while ((++heap)->start);
#else // #ifdef FRAGMENTED_HEAP
    // Save free pointer.
    heap_free = d;
#endif

    // End symbol list.
    SYMBOL_NEXT(last_kept_sym) = nil;

    // Save last symbol for lookup_symbol().
    last_symbol = last_kept_sym;
}

// Relocate object pointer.
lispptr FASTCALL
relocate_ptr (char * x)
{
    // Sum up gap sizes up to the pointer.
    gapsize = 0;
    for (r = xlat_end; r != xlat;) {
        r -= sizeof (lispptr);
        if (*(char **) r > x)
            break;
        r -= sizeof (unsigned);
        gapsize += *(unsigned *) r;
    }

    // Subtract it from the pointer.
    return x - gapsize;
}

// Relocate object pointers on heap, stack, and in global vars.
void
relocate (void)
{
    // Relocate global pointers.
    for (gp = global_pointers; *gp; gp++)
        **gp = relocate_ptr (**gp);

#ifdef FRAGMENTED_HEAP
    heap = heaps;
    do {
        heap_start = heap->start;
#endif
#ifdef VERBOSE_GC
    out ('R');
    terpri ();
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

    // Relocate GC'ed stack.
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        *(lispptr *)p = relocate_ptr (*(lispptr *) p);
}

#ifdef FRAGMENTED_HEAP
// Switch to 'heap'.
void
switch_heap ()
{
    heap_start = heap->start;
    heap_free = heap->free;
    heap_end = heap->end;
    heap++;
}
#endif

// Mark and sweep objects, and relocate object pointers.
void
gc (void)
{
#ifdef FRAGMENTED_HEAP
    // Switch to next heap if available.
    if (heap->start) {
#ifdef VERBOSE_GC
        out ('N');
#endif
        goto next_heap;
    }
#endif

restart:
#ifdef FRAGMENTED_HEAP
    // Switch to first heap.
    heap = heaps;
    switch_heap ();
#endif

#ifdef VERBOSE_GC
    out ('M');
#endif

    // Mark global pointers.
    for (gp = global_pointers; *gp; gp++)
        mark (**gp);

    // Mark GC'ed stack.
    for (p = stack; p != stack_end; p += sizeof (lispptr))
        mark (*(lispptr *) p);

    sweep ();
    relocate ();

    // Restart if sweep was interrupted due
    // to full relocation table.
    if (xlat_full)
        goto restart;

#ifdef FRAGMENTED_HEAP
    // Switch to first heap to allocate from there.
    heap = heaps;
next_heap:
    switch_heap ();
#endif
}
