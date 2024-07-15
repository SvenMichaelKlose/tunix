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
test_triplet (lispptr o1, size_t s1, lispptr o2, lispptr o3, size_t s3)
{
    lispptr start = o1;
    lispptr end = o2 + s3;

    printf ("Pos o1: %d\n", o1 - start);
    printf ("Pos o2: %d\n", o2 - start);
    printf ("Pos o3: %d\n", o3 - start);
    printf ("Sizeof o1: %d\n", s1);
    printf ("Sizeof o3: %d\n", s3);
    SETCAR(root_cons, o1);
    SETCAR(root_cons, o3);
    gc ();
    printf ("%d\n", (lispptr) heap_free - end);
    assert((lispptr) heap_free == end);
}

void
test ()
{
    char * heap_fill;
    lispptr o1;
    lispptr o2;
    lispptr o3;

    // The universe only contains symbol T.
    heap_fill = heap_free;

    // Add an object, then GC and check if it only contains
    // T like before.  Do that with a cons, a number and a
    // symbol.
    make_number (0);
    gc ();
    assert(heap_fill == heap_free);

    // Make rooting cons for test objects.
    root_cons = make_cons (nil, nil);
    expand_universe (root_cons);
    heap_fill = heap_free;

    // Again for each object: first add an object that is
    // linked to the universe, then add the object that
    // will be removed, followed by another object that is
    // connected.
    // TOOD: Permutate over all types for each of the
    // three positions.

    o1 = make_cons (nil, nil);
    o2 = make_cons (nil, nil);
    o3 = make_cons (nil, nil);
    test_triplet (o1, sizeof (cons), o2, o3, sizeof (cons));
    outs ("Test 1 passed."); terpri ();

    o1 = make_number (1);
    o2 = make_number (2);
    o3 = make_number (3);
    test_triplet (o1, sizeof (number), o2, o3, sizeof (number));
    outs ("Test 2 passed."); terpri ();

    o1 = make_symbol ("1", nil);
    o2 = make_symbol ("2", nil);
    o3 = make_symbol ("3", nil);
    test_triplet (o1, sizeof (symbol) + 1, o2, o3, sizeof (symbol) + 1);
    outs ("Test 3 passed."); terpri ();
}
