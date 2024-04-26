#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "while.h"

addwhile (WHILE * ptr)
{
    if (while_table_index == WSTABSZ)
        return error ("too many active whiles");
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
    if (!while_table_index)
        return error ("no active do/for/while/switch");
    return &ws[while_table_index - 1];
}

WHILE *
findwhile ()
{
    int i;
    for (i = while_table_index; i--;)
        if (ws[i].type != WSSWITCH)
            return &ws[i];
    return error ("no active do/for/while");
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
        return error ("too many case labels");
    swstcase[swstp] = val;
    swstlab[swstp++] = lab = getlabel ();
    def_local (lab);
}
