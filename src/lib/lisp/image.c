#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>
#include <git-version.h>

image_header header;
lispptr ** tmpo;

#ifdef OVERLAY
#ifdef TARGET_VIC20
#pragma code-name (push, "OVL_COMMON")
#endif
#endif

bool FASTCALL
image_save (char * pathname)
{
    simpleio_chn_t old_chout = fnout;
    simpleio_chn_t chout;
    size_t len;

    // Open image file for writing.
    chout = simpleio_open (pathname, 'w');
    if (!chout)
        return false;
    setout (chout);

    // Make and write header.
    memcpy (&header.git_version, TUNIX_GIT_VERSION, sizeof (header.git_version));
    outm ((char *) &header, sizeof (header));

#ifdef FRAGMENTED_HEAP
    // Start with first heap.
    heap = heaps;
    do {
        switch_heap ();
#endif

        // Write heap start.
        outm ((char *) &heap_start, sizeof (lispptr));

        // Write heap size.
        len = heap_free - heap_start;
        outm ((char *) &len, sizeof (len));

        // Write heap data.
        outm (heap_start, len);
#ifdef FRAGMENTED_HEAP
    } while (heap->start);
#endif

    // Write global pointers.
    for (tmpo = global_pointers; *tmpo; tmpo++)
        outm ((char *) *tmpo, sizeof (lispptr));

    simpleio_close (chout);
    setout (old_chout);

#ifdef FRAGMENTED_HEAP
    // Switch to first heap.
    heap = heaps;
    switch_heap ();
#endif

    return true;
}

bool FASTCALL
image_load (char * pathname)
{
    simpleio_chn_t chin;
    size_t len;
    lispptr pos;

    chin = simpleio_open (pathname, 'r');
    if (!chin)
        return false;
    setin (chin);

    // Read header.
    inm ((char *) &header, sizeof (header));

    // Verify that the Git version matches.
    // NOTE: It must be exactly the same machine
    // used to save the image anyhow.
    if (strncmp ((char *) &header.git_version, TUNIX_GIT_VERSION, sizeof (header.git_version))) {
        simpleio_close (chin);
        return false;
    }

#ifdef FRAGMENTED_HEAP
    // Start with first heap.
    heap = heaps;
    do {
        switch_heap ();
#endif

        // Read heap address.
        inm ((char *) &pos, sizeof (lispptr));
        if (pos != heap_start)
            internal_error_ptr (pos, "position. ");

        // Read heap size.
        inm ((char *) &len, sizeof (len));

        // Read heap data.
        inm (heap_start, len);

        heap_free = heap_start + len;
        *heap_free = 0; // Mark end of heap.

#ifdef FRAGMENTED_HEAP
        heap->free = heap_free;
    } while (heap->start);
#endif

    // Read global pointers.
    for (tmpo = global_pointers; *tmpo; tmpo++)
        inm ((char *) *tmpo, sizeof (lispptr));

    // Close file.
    simpleio_close (chin);

    // Initialize stack pointers.
    stack    = stack_end;
    tagstack = tagstack_end;

    // GC to set up linked list of named symbols.
#ifdef FRAGMENTED_HEAP
    heap_free = heap_end;  // Ensure start with first heap.
#endif
    gc ();

    // Ensure standard I/O channels.
    set_channels (STDIN, STDOUT);

    return true;
}

#ifdef OVERLAY
#ifdef TARGET_VIC20
#pragma code-name (pop)
#endif
#endif
