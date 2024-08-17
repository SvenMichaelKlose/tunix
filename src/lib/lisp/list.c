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
    while (x) {
        lisp_len++;
        x = CDR(x);

        // Count CDR of dotted pair.
        if (x && ATOM(x)) {
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
    if (ATOM(x))
        return x;

    tmp = CDR(x);
#ifndef NAIVE
    if (tmp && ATOM(tmp)) {
        error_info = tmp;
        goto cons_expected;
    }
#endif

    if (mode == COPY_BUTLAST && NOT(tmp))
        return nil;

    // Remove first elements if they match 'needle'.
    if (mode == COPY_REMOVE) {
        while (CONSP(x) && needle == CAR(x))
            x = CDR(x);
    }

    // Copy first element.
    PUSH(x); // TODO: Explain why this is required. (smk)
    list_start = list_last = make_cons (CAR(x), nil);
    POP(x);

    // Append rest of elements.
    DOLIST(x, CDR(x)) {
        if (mode == COPY_BUTLAST && NOT(CDR(x)))
            goto end_butlast;

        // Skip element to remove.
        if (mode == COPY_REMOVE && needle == CAR(x))
            continue;

        // Copy element.
        PUSH(x); // TODO: Explain why this is required. (smk)
        tmp = make_cons (CAR(x), nil);
        POP(x);

        // Append to last.
        SETCDR(list_last, tmp);
        list_last = tmp;
    }

#ifndef NAIVE
    if (x) {
        error_info = x;
cons_expected:
        error (ERROR_TYPE, "not a cons");
        return nil;
    }
#endif

end_butlast:
    return list_start;
}

// Get last cons of a list.
lispptr FASTCALL
last (lispptr x)
{
    DOLIST(tmp2, x)
        if (ATOM(CDR(tmp2)))
            return tmp2;
    return nil;
}

// Get cons of list containing 'needle'.
lispptr FASTCALL
member (lispptr needle, lispptr x)
{
    DOLIST(tmp, x)
        if (CAR(tmp) == needle)
            return tmp;
    return nil;
}
