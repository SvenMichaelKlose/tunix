#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#ifndef __CC65__
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
extern lispptr args;
extern lispptr arg1;
extern lispptr arg2c;
extern lispptr arg2;
extern lispptr value;
extern lispptr tmp;
int len;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("value")
#pragma zpsym ("tmp")
#pragma zpsym ("len")
#pragma bss-name (pop)
#endif

lispptr list_start;
lispptr list_last;

int FASTCALL
length (lispptr x)
{
    len = 0;
    DOLIST(x, x)
        len++;
    return len;
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

lispptr FASTCALL
last (lispptr x)
{
    DOLIST(tmp2, x)
        if (ATOM(CDR(tmp2)))
            return tmp2;
    return nil;
}

lispptr FASTCALL
member (lispptr needle, lispptr haystack)
{
    DOLIST(x, haystack)
        if (CAR(x) == needle)
            return x;
    return nil;
}
