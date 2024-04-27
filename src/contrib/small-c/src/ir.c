#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "ir.h"
#include "primary.h"
#include "gen.h"
#include "ir-codes.h"

//////////////////////
/// INITIALIZATION ///
//////////////////////

void
initmac ()
{
}

////////////
/// FILE ///
////////////

// Print all assembler info before any
// code is generated.
void
header ()
{
    outb (IR_FILEHEAD);
}

void
trailer ()
{
    outb (IR_FILETAIL);
}

//////////////
/// OUTPUT ///
//////////////

void
output_label_terminator ()
{
    //outb (':');
}

void
gen_comment ()
{
    //outs ("; ");
}

/////////////////
/// OPERATORS ///
/////////////////

void
out1 (char code, int v)
{
    outb (code);
    outw (v);
}

void
out1s (char code, char *s)
{
    outb (code);
    outs (s);
}

void
_ldamc (char *s)
{
    out1s (IR_LDAMC, s);
}

void
_ldami (char *s)
{
    out1s (IR_LDAMI, s);
}

void
gen_ldaci (int v)
{
    out1 (IR_LDACI, v);
}

void
gen_ldbci (int v)
{
    out1 (IR_LDBCI, v);
}

void
gen_ldacig (char *s)
{
    out1s (IR_LDACIG, s);
}

void _addb () { outb (IR_ADDB); }
void _addsp () { outb (IR_ADDSP); }
void _callptr () { outb (IR_CALLPTR); }
void _compa () { outb (IR_COMPA); }
void _compb () { outb (IR_COMPB); }
void _inca () { outb (IR_INCA); }
void _deca () { outb (IR_DECA); }
void _decs1 () { outb (IR_DECS1); }
void _decs2 () { outb (IR_DECS2); }
void _incs1 () { outb (IR_INCS1); }
void _incs2 () { outb (IR_INCS2); }
void _getc () { outb (IR_GETC); }
void _geti () { outb (IR_GETI); }
void _getuc () { outb (IR_GETUC); }
void _signext () { outb (IR_SIGNEXT); }
void _sphl () { outb (IR_SPHL); }
void _stac () { outb (IR_SIGNEXT); }
void _stai () { outb (IR_SIGNEXT); }
void _popb () { outb (IR_POPB); }

////////////////
/// SEGMENTS ///
////////////////

void
gen_code_segment ()
{
    outb (IR_SEGCODE);
}

void
gen_data_segment ()
{
    outb (IR_SEGDATA);
}

////////////
/// DATA ///
////////////

void gen_datab () { outb (IR_DATAB); }
void gen_dataw () { outb (IR_DATAW); }
void gen_bss ()   { outb (IR_BSSB); }

//////////////
/// LABELS ///
//////////////

void
def_local (int nlab)
{
    out1 (IR_DEFLOCAL, nlab);
}

void
def_global (char *n)
{
    out1s (IR_DEFGLOBAL, n);
}

void
gen_local (int label)
{
    out1 (IR_LOCAL, label);
}

void
gen_global (char *n)
{
    out1s (IR_GLOBAL, n);
}

void
_gen_storage_decl (int do_export,
                   char *s)
{
    out1s (do_export ?
            IR_EXPORT :
            IR_IMPORT,
           s);
}

void
gen_decl_var (SYMBOL * s)
{
    if (symbol_table[current_symbol_table_idx].storage != STATIC)
        _gen_storage_decl (s->storage == EXTERN, s->name);
}

void
gen_decl_fun (SYMBOL * s)
{
    if (s->storage != STATIC)
        _gen_storage_decl (s->offset == FUNCTION, s->name);
}

/////////////////
/// REGISTERS ///
/////////////////

void gen_swap () { outb (IR_SWAP); }

int
gen_get_local (SYMBOL * sym)
{
    if (sym->storage == LSTATIC)
        gen_ldaci (sym->offset);
    else {
        gen_ldaci (sym->offset - stkp);
        _addsp ();
    }
    return REGA;
}

// Pointer increment
void
gen_ptrinc (LVALUE * lval)
{
    switch (lval->ptr_type) {
    case STRUCT:
        gen_ldbci (lval->tagsym->size);
        _addb ();
        break;
    case CINT:
    case UINT:
    default:
        _inca ();
        break;
    }
}

// Pointer decrement
void
gen_ptrdec (LVALUE * lval)
{
    _deca ();
    switch (lval->ptr_type) {
    case CINT:
    case UINT:
        _deca ();
        break;
    case STRUCT:
        gen_ldbci (lval->tagsym->size - 1);
        _compb ();
        _addb ();
        break;
    default:
        break;
    }
}

//////////////
/// MEMORY ///
//////////////

// Load memory cell at symbol into
// primary.
void
gen_get_memory (SYMBOL * sym)
{
    if (sym->identity != POINTER
        && sym->type == CCHAR) {
        _ldamc (sym->name);
        _signext ();
    } else if (sym->identity != POINTER
               && sym->type == UCHAR)
        _ldamc (sym->name);
    else
        _ldami (sym->name);
}

void
gen_put_memory (SYMBOL * sym)
{
    if (sym->identity != POINTER
        && (sym->type & CCHAR)) {
        _stac ();
    } else
        _stai ();
    outs (sym->name);
}

///////////////////
/// INDIRECTION ///
///////////////////

// Store the specified object type in
// primary at the address in the
// secondary register (on the top of the
// stack).
void
gen_put_indirect (int typeobj)
{
    gen_pop ();
    outb (typeobj & CCHAR ?
            IR_PUTC :
            IR_PUTI);
}

// Fetch the specified object type
// indirect through the primary register
// into the primary register.
void
gen_get_indirect (char typeobj, int reg)
{
    if (typeobj == CCHAR) {
        if (reg & REGB)
            gen_swap ();
        _getc ();
    } else if (typeobj == UCHAR) {
        if (reg & REGB)
            gen_swap ();
        _getuc ();
    } else
        _geti ();
}

/////////////
/// STACK ///
/////////////

void
gen_push (int reg)
{
    outb (reg & REGB ?
            IR_PUSHB :
            IR_PUSHA);
    stkp = stkp - INTSIZE;
}

// Pop into secondary.
void
gen_pop ()
{
    _popb ();
    stkp = stkp + INTSIZE;
}

// Swap primary and top of stack.
void
gen_swap_stack ()
{
    outb (IR_SWAPSTACK);
}

// Update stack pointer to new position.
int
gen_modify_stack (int newstkp)
{
    int k = newstkp - stkp;
    if (!k)
        return newstkp;
    if (k > 0) {
        // TODO: Remove this limit.
        if (k < 7) {
            if (k & 1) {
                _incs1 ();
                k--;
            }
            while (k) {
                _incs2 ();
                k = k - INTSIZE;
            }
            return newstkp;
        }
    } else {
        if (k > -7) {
            if (k & 1) {
                _decs1 ();
                k++;
            }
            while (k) {
                _decs2 ();
                k = k + INTSIZE;
            }
            return newstkp;
        }
    }
    gen_swap ();
    gen_ldaci (k);
    _addsp ();
    _sphl ();
    gen_swap ();
    return newstkp;
}

/////////////////
/// FUNCTIONS ///
/////////////////

void
gen_call (char *sname)
{
    outb (IR_CALL);
    outs (sname);
}

// Call with argument in secondary
// popped from stack.
void
out2 (char ir)
{
    gen_pop ();
    outb (ir);
}

void gen_ret () { outb (IR_RET); }

// Perform subroutine call to value on
// top of stack.
// TODO: Put it into an IR code.
void
callstk ()
{
    gen_ldaci (5); // TODO: ???
    gen_swap_stack ();
    _callptr ();
    stkp = stkp + INTSIZE;
}

/////////////
/// JUMPS ///
/////////////

void
gen_jump (int label)
{
    outb (IR_JMP);
    outw (label);
}

// 'case' jump
void
gen_jump_case ()
{
    outb (IR_JMPCASE);
}

// Test primary and jump to label.
// ft != 0: Jump if not zero.
// ft == 0: Jump if zero.
void
gen_test_jump (int label, int ft)
{
    outb (ft ? IR_JMPNZ : IR_JMPZ);
    outw (label);
}

/////////////
/// TRUTH ///
/////////////

void gen_tobool () { outb (IR_BOOL); }
void gen_not ()    { outb (IR_NOT); }

///////////////////
/// ARITHMETICS ///
///////////////////

void gen_neg ()  { outb (IR_NEG); }
void gen_mul2 () { outb (IR_ASL); }
void gen_div2 () { outb (IR_ASR); }

// Sum of primary and secondary
// If lval2 is int* and lval is not,
// scale lval.
void
gen_add (LVALUE *lval, LVALUE *lval2)
{
    gen_pop ();
    if (dbltest (lval2, lval)) {
        gen_swap ();
        gen_mul2 ();
        gen_swap ();
    }
    _addb ();
}

// Add offset to primary.
void
gen_add_const (int val)
{
    gen_ldbci (val);
    _addb ();
}

void gen_sub () { out2 (IR_SUB); }

// Multiply primary and secondary
// registers (result in primary)
void gen_mul () { out2 (IR_MUL); }

// Multiply primary by the length of
// some variable.
void
gen_mul_const (int type, int size)
{
    switch (type) {
    case CINT:
    case UINT:
        gen_mul2 ();
        break;
    case STRUCT:
        gen_ldbci (size);
        gen_mul ();
        break;
    default:
        break;
    }
}

// Divide the secondary register by
// primary.  Quotient in primary,
// remainder in secondary.
void gen_div () { out2 (IR_DIV); }

// Unsigned divide secondary register
// by the primary.  Quotient in primary,
// remainder in secondary.
void gen_udiv () { out2 (IR_UDIV); }

// Remainder (mod) of secondary register
// divided by the primary.  Remainder in
// primary, quotient in secondary.
void
gen_mod ()
{
    gen_div ();
    gen_swap ();
}

// Remainder (mod) of secondary divided
// by primary register.   Remainder in
// primary, quotient in secondary.
void
gen_umod ()
{
    gen_udiv ();
    gen_swap ();
}

////////////////////////
/// BIT MANIPULATION ///
////////////////////////

// Inclusive 'or' the primary and
// secondary registers.
void gen_or () { out2 (IR_OR); }

// Exclusive 'or' the primary and
// secondary registers.
void gen_xor () { out2 (IR_XOR); }

// 'and' primary and secondary.
void gen_and () { out2 (IR_AND); }

// One's complement of primary.
void gen_complement () { _compa (); }

//////////////
/// SHIFTS ///
//////////////

// Arithmetic shift right the secondary // register the number of times in the
// primary.  Result in primary.
void gen_asr () { out2 (IR_ASR); }

// Logically shift right the secondary
// register the number of times in the
// primary register.  Result in primary.
void gen_lsr () { out2 (IR_LSR); }

// Arithmetic shift left secondary
// register the number of times in the
// primary register.  Result in primary.
void gen_asl () { out2 (IR_ASL); }

////////////////////
/// CONDITIONALS ///
////////////////////

/*
  Following are the conditional
  operators.  They compare the secondary
  against the primary and put a literal
  1 in the primary if the condition is
  true, 0 otherwise.
 */

void gen_eq ()   { out2 (IR_EQ); }
void gen_neq ()  { out2 (IR_NE); }
void gen_lt ()   { out2 (IR_LT); }
void gen_lte ()  { out2 (IR_LTE); }
void gen_gt ()   { out2 (IR_GT); }
void gen_gte ()  { out2 (IR_GTE); }
void gen_ult ()  { out2 (IR_ULT); }
void gen_ulte () { out2 (IR_ULTE); }
void gen_ugt ()  { out2 (IR_UGT); }
void gen_ugte () { out2 (IR_UGTE); }

/////////////////
/// TOP-LEVEL ///
/////////////////

char *
inclib ()
{
#ifdef  INCDIR
    return INCDIR;
#else
    return "";
#endif
}
