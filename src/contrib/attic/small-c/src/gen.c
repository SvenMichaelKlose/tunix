#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "ir-codes.h"
#include "ir.h"
#include "expr.h"
#include "lex.h"
#include "primary.h"
#include "gen.h"

//////////////
/// OUTPUT ///
//////////////

char
outb (char c)
{
    fputc (c, output);
    return c;
}

void
outw (int v)
{
    fputc (v & 255, output);
    fputc (v >> 8, output);
}

void
outs (char *s)
{
    while (outb (*s++));
}

void
print_tab ()
{
    outb ('\t');
}

void
outtabs (char ptr[])
{
    print_tab ();
    outs (ptr);
}

void
newline ()
{
    outb (LF);
}

void
outl (char ptr[])
{
    outtabs (ptr);
    newline ();
}

void
outn (int number)
{
    fprintf (output, "%d", number);
}

//////////////
/// SOURCE ///
//////////////

void
gen_srcline (char *n)
{
    outb (IR_SRCLINE);
    outs (line);
    outb (0);
}

//////////////
/// LABELS ///
//////////////

// Return next available label number.
int
getlabel ()
{
    return nxtlab++;
}

////////////////////////
/// MEMORY LOCATIONS ///
////////////////////////

void
store (LVALUE * lval)
{
    if (!lval->indirect)
        gen_put_memory (lval->symbol);
    else
        gen_put_indirect (lval->indirect);
}

int
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
void
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

void
scale_const (int type, int otag,
             int *size)
{
    switch (type) {
    case CINT:
    case UINT:
        *size += *size;
        break;
    case STRUCT:
        *size *= tags[otag].size;
        break;
    default:
        break;
    }
}

////////////////////
/// ARITHMETHICS ///
////////////////////

void
gen_divide (LVALUE * a, LVALUE * b)
{
    if (nosign (a) || nosign (b))
        gen_udiv ();
    else
        gen_div ();
}

void
gen_modulo (LVALUE *a, LVALUE *b)
{
    if (nosign (a) || nosign (b))
        gen_umod ();
    else
        gen_mod ();
}

void
gen_ashiftr (LVALUE *lval)
{
    if (nosign (lval))
        gen_lsr ();
    else
        gen_asr ();
}

int
ptrsize (LVALUE *lval)
{
   return lval->tagsym ?
        lval->tagsym->size :
        INTSIZE;
}

void
gen_scaled_ptrop (void (*gen) (LVALUE *, LVALUE *),
                  LVALUE *a, LVALUE *b)
{
    if (dbltest (a, b))
        gen_mul_const (a->ptr_type, ptrsize (a));
    if (gen)
        gen (a, b);
}
