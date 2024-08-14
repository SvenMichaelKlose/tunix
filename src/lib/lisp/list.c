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

lispptr FASTCALL
copy_list (lispptr x, char mode, lispptr needle)
{
    if (ATOM(x))
        return x;
    if (mode == COPY_BUTLAST && !CDR(x))
        return nil;
    if (mode == COPY_REMOVE)
        while (needle == CAR(x)) {
            x = CDR(x);
            if (ATOM(x))
                return x;
        }
    PUSH(x);
    PUSH(needle);
    list_start = list_last = make_cons (CAR(x), nil);
    POP(needle);
    POP(x);
    PUSH(list_start);
    DOLIST(x, CDR(x)) {
        if (mode == COPY_BUTLAST && !CDR(x))
            break;
        if (mode == COPY_REMOVE && needle == CAR(x))
            continue;
        PUSH(list_last);
        PUSH(x);
        PUSH(needle);
        tmp = make_cons (CAR(x), nil);
        POP(needle);
        POP(x);
        POP(list_last);
        SETCDR(list_last, tmp);
        list_last = tmp;
    }

    if (x && ATOM(x))
        if (mode != COPY_REMOVE || needle != x)
            SETCDR(list_last, tmp);

    POP(list_start);
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
