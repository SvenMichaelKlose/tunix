#include <stdio.h>
#include "defs.h"
#include "data.h"

addwhile (WHILE * ptr)
{
    if (while_table_index == WSTABSZ) {
        error ("too many active whiles");
        return;
    }
    ws[while_table_index++] = *ptr;
}

delwhile ()
{
    if (readwhile ())
        while_table_index--;
}

WHILE *
readwhile ()
{
    if (while_table_index == 0) {
        error ("no active do/for/while/switch");
        return 0;
    }
    return &ws[while_table_index - 1];
}

WHILE *
findwhile ()
{
    int while_table_idx;
    while_table_idx = while_table_index;
    for (; while_table_idx != 0;) {
        while_table_idx--;
        if (ws[while_table_idx].type != WSSWITCH)
            return &ws[while_table_idx];
    }
    error ("no active do/for/while");
    return 0;
}

WHILE *
readswitch ()
{
    WHILE *ptr;
    if (ptr = readwhile ())
        if (ptr->type == WSSWITCH)
            return ptr;
    return 0;
}

addcase (int val)
{
    int lab;
    if (swstp == SWSTSZ)
        error ("too many case labels");
    else {
        swstcase[swstp] = val;
        swstlab[swstp++] = lab = getlabel ();
        def_local (lab);
    }
}
