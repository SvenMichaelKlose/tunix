#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

// Set input/output channels.
void FASTCALL
set_channels (simpleio_chn_t cin, simpleio_chn_t cout)
{
    arg1 = make_number (cin);
    bi_setin ();
    arg1 = make_number (cout);
    bi_setout ();
}
