#include <stdio.h>
#include <error.h>

#include "defs.h"
#include "data.h"
#include "ir.h"
#include "sym.h"
#include "while.h"

void
addwhile (WHILE * ptr)
{
    if (while_table_index == WSTABSZ)
        perror ("too many active whiles");
    ws[while_table_index++] = *ptr;
}

void
delwhile ()
{
    if (readwhile ())
        while_table_index--;
}

WHILE *
readwhile ()
{
    if (!while_table_index) {
        perror ("no active do/for/while/switch");
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
    perror ("no active do/for/while");
    return 0;
}

WHILE *
readswitch ()
{
    WHILE *ptr;
    if ((ptr = readwhile ()))
        if (ptr->type == WSSWITCH)
            return ptr;
    return 0;
}

void
addcase (int val)
{
    int lab;
    if (swstp == SWSTSZ) {
        perror ("too many case labels");
        return;
    }
    swstcase[swstp] = val;
    swstlab[swstp++] = lab = getlabel ();
    def_local (lab);
}
