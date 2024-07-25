// TODO: Tests compiled separately to run each one in
// a defined state.

#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char * heap_start;
extern char * heap_free;
#ifdef __CC65__
#pragma zpsym ("heap_start")
#pragma zpsym ("heap_free")
#pragma bss-name (pop)
#endif

cons * root_cons;

void
test_triplet (char *info, lispptr o1, size_t s1, lispptr o2, size_t s2, lispptr o3, size_t s3)
{
    char *start = o1;
    char *end = (char *) o2 + s3;
#ifdef NDEBUG
    (void) s2;
#endif

    printf (">>> Test: '%s'.\n", info);
    printf ("Pos o1: %ld\n", (char *) o1 - start);
    printf ("Sizeof o1: %ld\n", s1);
    printf ("Pos o2: %ld\n", (char *) o2 - start);
    assert((char *) o1 + s1 == o2);
    printf ("Pos o3: %ld\n", (char *) o3 - start);
    printf ("Sizeof o3: %ld\n", s3);
    assert((char *) o2 + s2 == o3);
    SETCAR(root_cons, o1);
    SETCDR(root_cons, o3);
    gc ();
    if ((heap_free - end))
        printf ("Difference to expected pointer address: %ld\n", heap_free - end);
    assert(heap_free == end);
    printf ("<<< Test '%s' passed.\n", info);
    SETCAR(root_cons, nil);
    SETCDR(root_cons, nil);
    gc ();
}

void
test ()
{
#ifndef NDEBUG
    char * heap_fill = heap_free;
#endif
    lispptr o1;
    lispptr o2;
    lispptr o3;

    // Add an object, then GC and check if it only contains
    // T like before.  Do that with a cons, a number and a
    // symbol.
    make_number (0);
    gc ();
    assert(heap_fill == heap_free);

    // Make rooting cons for test objects.
    root_cons = make_cons (nil, nil);
    expand_universe (root_cons);

    // Again for each object: first add an object that is
    // linked to the universe, then add the object that is not
    // linked to the root_cons and will be removed, followed by
    // another object that is connected.
    // TOOD: Permutate over all combinations of types for each
    // of the three positions.

    o1 = make_cons (nil, nil);
    o2 = make_cons (nil, nil);
    o3 = make_cons (nil, nil);
    test_triplet ("conses", o1, sizeof (cons), o2, sizeof (cons), o3, sizeof (cons));

    o1 = make_number (1);
    o2 = make_number (2);
    o3 = make_number (3);
    test_triplet ("numbers", o1, sizeof (number), o2, sizeof (number), o3, sizeof (number));

    o1 = make_symbol ("1", 1);
    o2 = make_symbol ("2", 1);
    o3 = make_symbol ("3", 1);
    test_triplet ("symbols", o1, sizeof (symbol) + 1, o2, sizeof (symbol) + 1, o3, sizeof (symbol) + 1);
}
