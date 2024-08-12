#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>
#ifndef __CC65__
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>
#include <git-version.h>

image_header header;
lispptr ** tmpo;

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

    // Write heap size.
    len = heap_free - heap_start;
    outm ((char *) &len, sizeof (len));
    // Write heap data.
    outm (heap_start, heap_free - heap_start);

    // Write global pointers.
    for (tmpo = global_pointers; *tmpo; tmpo++)
        outm ((char *) *tmpo++, sizeof (lispptr));

    simpleio_close (chout);
    setout (old_chout);

    return true;
}

bool FASTCALL
image_load (char * pathname)
{
    simpleio_chn_t chin;
    size_t len;

    chin = simpleio_open (pathname, 'r');
    if (!chin)
        return false;
    setin (chin);

    inm ((char *) &header, sizeof (header));
    if (strncmp ((char *) &header.git_version, TUNIX_GIT_VERSION, sizeof (header.git_version))) {
        simpleio_close (chin);
        return false;
    }

    // Read heap.
    inm ((char *) &len, sizeof (len));
    inm (heap_start, len);
    heap_free = heap_start + len;
    *heap_free = 0; // Mark end of heap.

    // Read global pointers.
    for (tmpo = global_pointers; *tmpo; tmpo++)
        inm ((char *) *tmpo++, sizeof (lispptr));

    simpleio_close (chin);
    set_channels (STDIN, STDOUT);

    stack = stack_end;
    tagstack = tagstack_end;

    // GC to set up linked list of named symbols.
    gc ();
    return true;
}
