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

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr list_start; // Start of list.
lispptr list_last;  // Last cons of list.
#ifdef __CC65__
#pragma bss-name (pop)
#endif

#ifdef __CC65__
#pragma code-name ("CODE_LIST")
#endif

// Get number of list elements.
// CDRs of dotted pairs are also counted.
int FASTCALL
length (lispptr x)
{
    lisp_len = 0;
    while (NOT_NIL(x)) {
        lisp_len++;
        x = CDR(x);

        // Count CDR of dotted pair.
        if (NOT_NIL(x) && ATOM(x)) {
            lisp_len++;
            break;
        }
    }
    return lisp_len;
}

lispptr needle;

lispptr FASTCALL
copy_list (lispptr x, char mode, lispptr needle)
{
    tmp2 = x;

    if (ATOM(tmp2))
        return tmp2;

    tmp = CDR(tmp2);
#ifndef NAIVE
    if (NOT_NIL(tmp) && ATOM(tmp))
        return error_cons_expected (tmp);
#endif

    if (mode == COPY_BUTLAST && NOT(tmp))
        return nil;

    // Remove first elements if they match 'needle'.
    if (mode == COPY_REMOVE)
        while (CONSP(tmp2) && needle == CAR(tmp2))
            tmp2 = CDR(tmp2);

    // Copy first element.
    list_start = list_last = make_cons (CAR(tmp2), nil);

    // Append rest of elements.
    DOLIST(tmp2, CDR(tmp2)) {
        if (mode == COPY_BUTLAST && NOT(CDR(tmp2)))
            goto end_butlast;

        // Skip element to remove.
        if (mode == COPY_REMOVE && needle == CAR(tmp2))
            continue;

        // Copy and append cons.
        tmp = make_cons (CAR(tmp2), nil);
        SETCDR(list_last, tmp);
        list_last = tmp;
    }

#ifndef NAIVE
    if (NOT_NIL(tmp2))
        return error_cons_expected (tmp2);
#endif

end_butlast:
    tmp2 = nil;
    return list_start;
}

// Get last cons of a list.
lispptr FASTCALL
last (lispptr x)
{
    DOLIST(tmp2, x)
        if (ATOM(CDR(tmp2)))
            return tmp2;
#ifndef NAIVE
    if (NOT_NIL(tmp2))
        return error_cons_expected (tmp2);
#endif
    return nil;
}

// Get cons of list containing 'needle'.
// For internal use only as it ignores CDRs of dotted pairs.
lispptr FASTCALL
member (lispptr needle, lispptr x)
{
    DOLIST(tmp, x)
        if (CAR(tmp) == needle)
            return tmp;
    return nil;
}

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

void
init_list (void)
{
    list_start = list_last = nil;
}
