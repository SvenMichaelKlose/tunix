#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef TARGET_UNIX
#include <stdio.h>
#include <strings.h>
#endif

extern lispptr lisp_fnin;
extern lispptr lisp_fnout;

#ifdef COMPRESSED_CONS
bool do_compress_cons;
#endif

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
    &breakpoints_sym,
    &debugger_return_value_sym,
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

xlat_item * xlat_end;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif

lispobj_size_t n;

char * s;   // Source
char * d;   // Destination

xlat_item * xlat;
xlat_item * xlat_start;
bool        xlat_full;
char *      last_sweeped; // For merging consecutive gaps.
size_t      gapsize;

char *      p;
xlat_item * r;

#ifdef __CC65__
#pragma zpsym ("n")
#pragma zpsym ("s")
#pragma zpsym ("d")
#pragma zpsym ("xlat")
#pragma zpsym ("xlat_start")
#pragma zpsym ("xlat_full")
#pragma zpsym ("gapsize")
#pragma zpsym ("p")
#pragma zpsym ("r")
#pragma bss-name (pop)
#endif

#ifdef FRAGMENTED_HEAP
size_t total_removed;
#endif

void FASTCALL
add_gap (lispobj_size_t n)
{
    // Log gap position and size.
    xlat--;
    xlat->pos  = s;
    xlat->size = n;

#ifdef FRAGMENTED_HEAP
    total_removed += n;
#endif
}

// Copy marked objects over deleted ones and make a
// relocation table containing the addresses and sizes of
// the deleted objects.
void
sweep ()
{
#ifdef FRAGMENTED_HEAP
    // Start with first heap.
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
        heap_free  = heap->free;
#ifdef PARANOID
        heap_end   = heap->end;
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
#ifdef COMPRESSED_CONS
                // Turn regular cons into compressed cons...
                else if (do_compress_cons && !xlat_full && _CONSP(s) && !_EXTENDEDP(s)) {
                    // ...if CDR is pointing to the following object.
                    if (CONS(s)->cdr == s + sizeof (cons)) {
#ifdef VERBOSE_COMPRESSED_CONS
                        out ('C');
#endif

                        // Copy with mark bit cleared and type extended.
                        *d = (*s & ~TYPE_MARKED) | TYPE_EXTENDED;

                        // Copy CAR.
                        CONS(d)->car = CONS(s)->car;

                        // Advance.
                        d += sizeof (ccons);
                        s += sizeof (cons);

                        // Add gap to relocation table.
                        add_gap (sizeof (cons) - sizeof (ccons));

                        goto check_xlat;
                    }
                }
#endif // #ifdef COMPRESSED_CONS

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
                    xlat->size += n;
#ifdef FRAGMENTED_HEAP
                    total_removed += n;
#endif
                } else {
                    last_sweeped = d;
                    add_gap (n);
                }

                // Step to next object.
                s += n;

#ifdef COMPRESSED_CONS
check_xlat:
#endif
#ifndef PARANOID
                if (xlat < xlat_start)
                    // Reloc table size must be multiple of entry size!
                    internal_error ("xlat overflow");
#endif
                // Flag is relocation table is full, so
                // the rest of the objects won't be sweeped
                // and GC is started again.
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
        xlat--;
        xlat->pos  = s;
        xlat->size = -total_removed;
        if (xlat == xlat_start)
            xlat_full = true;
#ifdef VERBOSE_GC
        outn (total_removed); outs ("B freed."); terpri ();
#endif
    } while ((++heap)->start);
#else // #ifdef FRAGMENTED_HEAP
    // Save free pointer.
    heap_free = d;
#endif

#ifndef NDEBUG
    bzero (d, heap_end - d);
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
        r--;
        if (r->pos > (lispptr) x)
            break;
        gapsize += r->size;
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
#ifdef PARANOID
            if (p >= heap_end)
                internal_error ("Reloc: heap overflow");
#endif
            if (_CONSP(p)) {
                SETCAR(p, relocate_ptr (CAR(p)));
#ifdef COMPRESSED_CONS
                if (!_EXTENDEDP(p))
#endif
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

size_t
heap_free_size ()
{
#ifdef FRAGMENTED_HEAP
    struct heap_fragment *h;
    size_t freed = 0;
    for (h = heaps; h->start; h++)
        freed += h->end - h->free;
    return freed;
#else
    return heap_end - heap_free;
#endif
}

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
    for (p = stack; p != stack_end; p += sizeof (lispptr)) {
        //printf ("Mark stack %p: obj %p\n", p, *(lispptr *) p);
        mark (*(lispptr *) p);
    }

    sweep ();
    relocate ();

    // Restart if sweep was interrupted due
    // to full relocation table.
    if (xlat_full) {
#ifdef VERBOSE_GC
        outs ("!GC restart!");
#endif
        goto restart;
    }

#ifdef FRAGMENTED_HEAP
    // Switch to first heap to allocate from there.
    heap = heaps;
next_heap:
    switch_heap ();
#endif
}

// Address of moved-up heap.
char * heap_old;
size_t heap_used;
size_t heap_unused;

bool
compression_needs_break (void)
{
    // Skip what's already been dealt with.
    if (!s || MARKED(s))
        return true;

    // Stop if relocation table is full.
    if (xlat == xlat_start) {
        // Mark for post-GC.
        mark (s);
        return true;
    }

    return false;
}

void compress_object (void);

void
compress_list (void)
{
    list_start = d;

next_cons:
    if (compression_needs_break ())
        return;

    // Save relocation info.
    xlat--;
    xlat->pos  = s;
    xlat->size = d - s; // TODO

    // Copy regular cons if CDR is an atom that's been
    // copied already or is NIL.
    tmp = CDR(s);
    if (ATOM(tmp) && (!tmp || MARKED(tmp))) {
        n = sizeof (cons);
        while (n--)
            *d++ = *s++;
        goto process_cars;
    }

    // Copy as compressed cons type and CAR.
    *d = *s | TYPE_MARKED | TYPE_EXTENDED;
    CONS(d)->car = CONS(s)->car;

    // Handle atomic CDR.
    if (ATOM(tmp)) {
        s = tmp;
        compress_object ();
        goto process_cars;
    }
    goto next_cons;

process_cars:
    p = list_start;
    while (1) {
        // Compress CAR list.
        tmp = CAR(p);
        if (CONSP(tmp)) {
            PUSH(p);
            s = CAR(p);
            compress_object ();
            POP(p);
        }

        // Break on end of list.
        if (ATOM(CDR(p)))
            return;

        p = CDR(p);
    }
}

void
compress_object (void)
{
restart:
    if (compression_needs_break ())
        return;

    if (CONSP(s))
        compress_list ();
    else {
        n = objsize (s);

        // Stop if we're out of space.
        if (d + n >= heap_old)
            return;

        // Memorize symbol value.
        p = nil;
        if (_NAMEDP(s))
            p = SYMBOL_VALUE(s);

        // Add relocation info.
        xlat--;
        xlat->pos  = s;
        xlat->size = s - d;  // TODO

        // Copy object marked.
        *d++ = *s++ | TYPE_MARKED;
        while (n--)
            *d++ = *s++;

        // Continue with symbol value.
        if (p) {
            s = p;
            goto restart;
        }
    }
}

void
move_pointers (char * start, size_t len)
{
    start++, len++;
    return;
}

size_t c;

void
compress (void)
{
    // Call garbage collector for compacting.
    gc ();

    // Calculate areas and sizes of old and new heap.
    heap_old    = heap_start + heap_used;
    heap_unused = heap_end - heap_free;
    heap_used   = heap_free - heap_start;

    // Move used heap part up to the end.
    d = heap_end;
    s = heap_free;
    c = heap_used;
    while (c--)
        *--d = *--s;

    move_pointers (heap_old, heap_unused);

    // Start with list *universe*.
    s = universe;
    d = heap_start;
    compress_object ();

    // Append old heap to copied one.
    // TODO: Skip this step by leaving a gap of minimum size
    // that can be filled with unused dummy objects.
    memcpy (d, heap_old, heap_used);
    move_pointers (heap_start, (size_t) (d - heap_start) + heap_used);

    relocate ();
}
