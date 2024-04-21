#include <stdio.h>
#include "defs.h"
#include "data.h"

//////////////
/// OUTPUT ///
//////////////

outb (char c)
{
    if (!c)
        return 0;
    fputc (c, output);
    return c;
}

outs (char ptr[])
{
    int k;
    k = 0;
    while (outb (ptr[k++]));
}

print_tab ()
{
    outb ('\t');
}

outtabs (char ptr[])
{
    print_tab ();
    outs (ptr);
}

newline ()
{
    outb (LF);
}

outl (char ptr[])
{
    outtabs (ptr);
    newline ();
}

outn (int number)
{
    fprintf (output, "%d", number);
}

//////////////
/// LABELS ///
//////////////

// Return next available label number.
getlabel ()
{
    return nxtlab++;
}

////////////////////////
/// MEMORY LOCATIONS ///
////////////////////////

store (LVALUE * lval)
{
    if (!lval->indirect)
        gen_put_memory (lval->symbol);
    else
        gen_put_indirect (lval->indirect);
}

rvalue (LVALUE * lval, int reg)
{
    if (lval->symbol && !lval->indirect)
        gen_get_memory (lval->symbol);
    else
        gen_get_indirect (lval->indirect, reg);
    return REGA;
}

////////////
/// TEST ///
////////////

// Parses test part "(expression)"
// input and generates assembly for
// jump.
// @param ft : false: test if false,
//             true:  test if true
test (int label, int ft)
{
    needbrack ("(");
    expression (YES);
    needbrack (")");
    gen_test_jump (label, ft);
}

///////////////////////
/// TYPE CONVERSION ///
///////////////////////

scale_const (int type, int otag,
             int *size)
{
    switch (type) {
    case CINT:
    case UINT:
        *size += *size;
        break;
    case STRUCT:
        *size *= tag_table[otag].size;
        break;
    default:
        break;
    }
}
