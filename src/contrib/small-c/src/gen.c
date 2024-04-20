#include <stdio.h>
#include "defs.h"
#include "data.h"

////////////////////
/// BASIC OUTPUT ///
////////////////////

output_byte (char c)
{
    if (c == 0)
        return 0;
    fputc (c, output);
    return c;
}

output_string (char ptr[])
{
    int k;
    k = 0;
    while (output_byte (ptr[k++]));
}

print_tab ()
{
    output_byte ('\t');
}

output_with_tab (char ptr[])
{
    print_tab ();
    output_string (ptr);
}

output_line (char ptr[])
{
    output_with_tab (ptr);
    newline ();
}

output_decimal (int number)
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

def_local (int nlab)
{
    output_decimal (nlab);
    output_label_terminator ();
    newline ();
}

def_global (char *n)
{
    output_string (n);
    output_label_terminator ();
    newline ();
}

gen_local (int label)
{
    output_label_prefix ();
    output_decimal (label);
}

gen_global (char *n)
{
    output_string (n);
}

////////////////////////
/// MEMORY LOCATIONS ///
////////////////////////

store (LVALUE * lval)
{
    if (lval->indirect == 0)
        gen_put_memory (lval->symbol);
    else
        gen_put_indirect (lval->indirect);
}

rvalue (LVALUE * lval, int reg)
{
    if ((lval->symbol != 0) & (lval->indirect == 0))
        gen_get_memory (lval->symbol);
    else
        gen_get_indirect (lval->indirect, reg);
    return HL_REG;
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
