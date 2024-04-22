#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "ir.h"
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
header ()
{
    outb (IR_FILEHEAD);
}

trailer ()
{
    outb (IR_FILETAIL);
}

//////////////
/// OUTPUT ///
//////////////

output_label_terminator ()
{
    //outb (':');
}

gen_comment ()
{
    //outs ("; ");
}

/////////////////
/// OPERATORS ///
/////////////////

out1 (char code, int v)
{
    outb (code);
    outw (v);
}

out1s (char code, char *s)
{
    outb (code);
    outs (s);
}

_ldamc (char *s)
{
    out1s (IR_LDAMC, s);
}

_ldami (char *s)
{
    out1s (IR_LDAMI, s);
}

gen_ldaci (int v)
{
    out1 (IR_LDACI, v);
}

gen_ldbci (int v)
{
    out1 (IR_LDBCI, v);
}

_addb () { outb (IR_ADDB); }
_addsp () { outb (IR_ADDSP); }
_callptr () { outb (IR_CALLPTR); }
_compa () { outb (IR_COMPA); }
_compb () { outb (IR_COMPB); }
_inca () { outb (IR_INCA); }
_deca () { outb (IR_DECA); }
_decs1 () { outb (IR_DECS1); }
_decs2 () { outb (IR_DECS2); }
_incs1 () { outb (IR_INCS1); }
_incs2 () { outb (IR_INCS2); }
_getc () { outb (IR_GETC); }
_geti () { outb (IR_GETI); }
_getuc () { outb (IR_GETUC); }
_signext () { outb (IR_SIGNEXT); }
_sphl () { outb (IR_SPHL); }
_stac () { outb (IR_SIGNEXT); }
_stai () { outb (IR_SIGNEXT); }
_popb () { outb (IR_POPB); }

////////////////
/// SEGMENTS ///
////////////////

gen_code_segment ()
{
    outb (IR_SEG_CODE);
}

gen_data_segment ()
{
    outb (IR_SEG_DATA);
}

////////////
/// DATA ///
////////////

gen_datab () { outb (IR_DATAB); }
gen_dataw () { outb (IR_DATAW); }
gen_bss ()   { outb (IR_BSSB); }

//////////////
/// LABELS ///
//////////////

def_local (int nlab)
{
    out1 (IR_DEFLOCAL, nlab);
}

def_global (char *n)
{
    out1s (IR_DEFGLOBAL, n);
}

gen_local (int label)
{
    out1 (IR_LOCAL, label);
}

gen_global (char *n)
{
    out1s (IR_GLOBAL, n);
}

_gen_storage_decl (int do_export,
                   char *s)
{
    out1s (do_export ?
            IR_EXPORT :
            IR_IMPORT,
           s);
}

gen_decl_var (SYMBOL * s)
{
    if (symbol_table[current_symbol_table_idx].storage != STATIC)
        _gen_storage_decl (s->storage == EXTERN, s->name);
}

gen_decl_fun (SYMBOL * s)
{
    if (s->storage != STATIC)
        _gen_storage_decl (s->offset == FUNCTION, s->name);
}

/////////////////
/// REGISTERS ///
/////////////////

gen_swap () { outb (IR_SWAP); }

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
gen_put_indirect (char typeobj)
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

gen_push (int reg)
{
    outb (reg & REGB ?
            IR_PUSHB :
            IR_PUSHA);
    stkp = stkp - INTSIZE;
}

// Pop into secondary.
gen_pop ()
{
    _popb ();
    stkp = stkp + INTSIZE;
}

// Swap primary and top of stack.
gen_swap_stack ()
{
    outb (IR_SWAPSTACK);
}

// Update stack pointer to new position.
gen_modify_stack (int newstkp)
{
    int k;

    k = newstkp - stkp;
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

gen_call (char *sname)
{
    outb (IR_CALL);
    outs (sname);
}

// Call with argument in secondary
// popped from stack.
out2 (char ir)
{
    gen_pop ();
    outb (ir);
}

gen_ret () { outb (IR_RET); }

// Perform subroutine call to value on
// top of stack.
// TODO: Put it into an IR code.
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

gen_jump (int label)
{
    outb (IR_JMP);
    outw (label);
}

// 'case' jump
gen_jump_case ()
{
    outb (IR_JMPCASE);
}

// Test primary and jump to label.
// ft != 0: Jump if not zero.
// ft == 0: Jump if zero.
gen_test_jump (int label, int ft)
{
    outb (ft ? IR_JMPNZ : IR_JMPZ);
    outw (label);
}

/////////////
/// TRUTH ///
/////////////

gen_tobool () { outb (IR_BOOL); }
gen_not ()    { outb (IR_NOT); }

///////////////////
/// ARITHMETICS ///
///////////////////

gen_neg ()  { outb (IR_NEG); }
gen_mul2 () { outb (IR_ASL); }
gen_div2 () { outb (IR_ASR); }

// Sum of primary and secondary
// If lval2 is int* and lval is not,
// scale lval.
gen_add (int *lval, int *lval2)
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
gen_add_const (int val)
{
    gen_ldbci (val);
    _addb ();
}

gen_sub () { out2 (IR_SUB); }

// Multiply primary and secondary
// registers (result in primary)
gen_mul () { out2 (IR_MUL); }

// Multiply primary by the length of
// some variable.
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
gen_div () { out2 (IR_DIV); }

// Unsigned divide secondary register
// by the primary.  Quotient in primary,
// remainder in secondary.
gen_udiv () { out2 (IR_UDIV); }

// Remainder (mod) of secondary register
// divided by the primary.  Remainder in
// primary, quotient in secondary.
gen_mod ()
{
    gen_div ();
    gen_swap ();
}

// Remainder (mod) of secondary divided
// by primary register.   Remainder in
// primary, quotient in secondary.
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
gen_or () { out2 (IR_OR); }

// Exclusive 'or' the primary and
// secondary registers.
gen_xor () { out2 (IR_XOR); }

// 'and' primary and secondary.
gen_and () { out2 (IR_AND); }

// One's complement of primary.
gen_complement () { _compa (); }

//////////////
/// SHIFTS ///
//////////////

// Arithmetic shift right the secondary // register the number of times in the
// primary.  Result in primary.
gen_asr () { out2 (IR_ASR); }

// Logically shift right the secondary
// register the number of times in the
// primary register.  Result in primary.
gen_lsr () { out2 (IR_LSR); }

// Arithmetic shift left secondary
// register the number of times in the
// primary register.  Result in primary.
gen_asl () { out2 (IR_ASL); }

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

gen_eq ()   { out2 (IR_EQ); }
gen_neq ()  { out2 (IR_NE); }
gen_lt ()   { out2 (IR_LT); }
gen_lte ()  { out2 (IR_LTE); }
gen_gt ()   { out2 (IR_GT); }
gen_gte ()  { out2 (IR_GTE); }
gen_ult ()  { out2 (IR_ULT); }
gen_ulte () { out2 (IR_ULTE); }
gen_ugt ()  { out2 (IR_UGT); }
gen_ugte () { out2 (IR_UGTE); }

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
