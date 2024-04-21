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
    if (!while_table_index) {
        error ("no active do/for/while/switch");
        return 0;
    }
    return &ws[while_table_index - 1];
}

WHILE *
findwhile ()
{
    int i;
    for (i = while_table_index; i--;)
        if (ws[i].type != WSSWITCH)
            return &ws[i];
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
