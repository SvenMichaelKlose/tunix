#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr arg1;
#ifdef __CC65__
#pragma zpsym ("arg1")
#pragma bss-name (pop)
#endif

void FASTCALL
set_channels (simpleio_chn_t cin, simpleio_chn_t cout)
{
    arg1 = make_number (cin);
    bi_setin ();
    arg1 = make_number (cout);
    bi_setout ();
}
