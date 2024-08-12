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

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr list_start; // Start of list.
lispptr list_last;  // Last cons of list.
#ifdef __CC65__
#pragma zpsym ("list_start")
#pragma zpsym ("list_last")
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
    list_start = list_last = make_cons (CAR(x), nil);
    POP(x);
    PUSH(list_start);
    DOLIST(x, CDR(x)) {
        if (CONSP(x)) {
            if (mode == COPY_BUTLAST && !CDR(x))
                break;
            if (mode != COPY_REMOVE || needle != CAR(x)) {
                PUSH(list_last);
                PUSH(x);
                tmp = make_cons (CAR(x), nil);
                POP(x);
                POP(list_last);
            }
        } else
            tmp = x;
        SETCDR(list_last, tmp);
        list_last = tmp;
    }
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
