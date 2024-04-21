// Becoming an IR code generator.

#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "ir.h"

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
}

trailer ()
{
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

gen_datab ()
{
    outb (IR_DATAB);
}

gen_dataw ()
{
    outb (IR_DATAW);
}

gen_bss ()
{
    outb (IR_BSSB);
}

//////////////
/// LABELS ///
//////////////

def_local (int nlab)
{
    outb (IR_DEFLOCAL);
    outw (nlab);
}

def_global (char *n)
{
    outb (IR_DEFGLOBAL);
    outs (n);
    outb (0);
}

gen_local (int label)
{
    outb (IR_LOCAL);
    outw (label);
}

gen_global (char *n)
{
    outb (IR_GLOBAL);
    outs (n);
    outb (0);
}

_gen_storage_decl (int do_import,
                   char *n)
{
    outb (
        do_import ?
            IR_IMPORT :
            IR_EXPORT
    );
    outs (n);
    outb (0);
}

gen_decl_var (SYMBOL * scptr)
{
    if (symbol_table[current_symbol_table_idx].storage == STATIC)
        return;
    _gen_storage_decl (scptr->storage == EXTERN, scptr->name);
}

gen_decl_fun (SYMBOL * scptr)
{
    if (scptr->storage == STATIC)
        return;
    _gen_storage_decl (scptr->offset == FUNCTION, scptr->name);
}

/////////////////
/// REGISTERS ///
/////////////////

gen_swap ()
{
    outb (IR_SWAP);
}

gen_load_1st ()
{
    outb (IR_LDA);
}

_gen_load_2nd_const (int v)
{
    outb (IR_LDB);
    outw (v);
}

gen_get_local (SYMBOL * sym)
{
    if (sym->storage == LSTATIC) {
        gen_load_1st ();
        gen_local (sym->offset);
    } else {
        gen_load_1st ();
        //TODO: gen_load_1st_const ();
        outw (sym->offset - stkp);
        outb (IR_ADDSP);
    }
    return REGA;
}

// Pointer increment
gen_ptrinc (LVALUE * lval)
{
    switch (lval->ptr_type) {
    case STRUCT:
        _gen_load_2nd_const (lval->tagsym->size);
        outb (IR_ADDB);
        break;
    case CINT:
    case UINT:
    default:
        outb (IR_INCA);
        break;
    }
}

// Pointer decrement
gen_ptrdec (LVALUE * lval)
{
    outb (IR_DECA);
    switch (lval->ptr_type) {
    case CINT:
    case UINT:
        outb (IR_DECA);
        break;
    case STRUCT:
        _gen_load_2nd_const (lval->tagsym->size - 1);
        outb (IR_COMPB);
        outb (IR_ADDB);
        break;
    default:
        break;
    }
}

//////////////
/// MEMORY ///
//////////////

gen_get_memory (SYMBOL * sym)
{
    if (sym->identity != POINTER
        && sym->type == CCHAR) {
        outb (IR_LDAL);
        outs (sym->name);
        outb (0);
        outb ("IR_SIGNEXT");
    } else if (sym->identity != POINTER
               && sym->type == UCHAR) {
        outb (IR_LDAL);
        outs (sym->name);
        outb (0);
    } else {
        outb (IR_LDA);
        outs (sym->name);
        outb (0);
    }
}

void
gen_put_memory (SYMBOL * sym)
{
    if (sym->identity != POINTER
        && (sym->type & CCHAR)) {
        outb (IR_STAL);
    } else
        outb (IR_STA);
    outs (sym->name);
    outb (0);
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
            IR_PUTCHAR :
            IR_PUTINT);
}

// Fetch the specified object type
// indirect through the primary register
// into the primary register.
void
gen_get_indirect (char typeobj, int reg)
{
    if (typeobj == CCHAR) {
        if (reg & REGB)
            outb (IR_SWAP);
        outb (IR_GETCHAR);
    } else if (typeobj == UCHAR) {
        if (reg & REGB)
            gen_swap ();
        outb (IR_GETUCHAR);
    } else
        outb (IR_GETINT);
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
    outb (IR_POPB);
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
        if (k < 7) {
            if (k & 1) {
                outb (IR_INCS1);
                k--;
            }
            while (k) {
                outb (IR_INCS2);
                k = k - INTSIZE;
            }
            return newstkp;
        }
    } else {
        if (k > -7) {
            if (k & 1) {
                outb (IR_DECS1);
                k++;
            }
            while (k) {
                outb (IR_DECS2);
                k = k + INTSIZE;
            }
            return newstkp;
        }
    }
    gen_swap ();
    gen_load_1st ();
    outw (k);
    outb (IR_ADDSP);
    outb (IR_SPHL);
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
    outb (0);
}

// Call with argument in secondary
// popped from stack.
outb1 (char ir)
{
    gen_pop ();
    outb (ir);
}

gen_ret ()
{
    outb (IR_RET);
}

// Perform subroutine call to value on
// top of stack.
// TODO: Put it into an IR code.
callstk ()
{
    gen_load_1st ();
    //outs ("#.+5");
    gen_swap_stack ();
    outb (IR_CALLPTR);
    stkp = stkp + INTSIZE;
}

// Squirrel away argument count in a
// register that modstk doesn't touch.
gnargs (int d)
{
}

/////////////
/// JUMPS ///
/////////////

gen_jump (int label)
{
    outb (IR_JMP);
    gen_local (label);
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
    gen_local (label);
}

/////////////
/// TRUTH ///
/////////////

gen_tobool ()
{
    outb (IR_BOOL);
}

gen_not ()
{
    outb (IR_LNEG);
}

///////////////////
/// ARITHMETICS ///
///////////////////

gen_neg ()
{
    outb (IR_NEG);
}

gen_mul2 ()
{
    outb (IR_ASL);
}

gen_div2 ()
{
    outb (IR_ASR);
}

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
    outb (IR_ADDB);
}

// Add offset to primary.
gen_add_const (int val)
{
    _gen_load_2nd_const (val);
    outb (IR_ADDB);
}

gen_sub ()
{
    outb1 (IR_SUB);
}

// Multiply primary and secondary
// registers (result in primary)
gen_mul ()
{
    outb1 (IR_MUL);
}

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
        _gen_load_2nd_const (size);
        gen_mul ();
        break;
    default:
        break;
    }
}

// Divide the secondary register by
// primary.  Quotient in primary,
// remainder in secondary.
gen_div ()
{
    outb1 (IR_DIV);
}

// Unsigned divide secondary register
// by the primary.  Quotient in primary,
// remainder in secondary.
gen_udiv ()
{
    outb1 (IR_UDIV);
}

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
gen_or ()
{
    outb1 (IR_OR);
}

// Exclusive 'or' the primary and
// secondary registers.
gen_xor ()
{
    outb1 (IR_XOR);
}

// 'and' primary and secondary.
gen_and ()
{
    outb1 (IR_AND);
}

// One's complement of primary.
gen_complement ()
{
    outb (IR_COMPA);
}

//////////////
/// SHIFTS ///
//////////////

// Arithmetic shift right the secondary // register the number of times in the
// primary.  Result in primary.
gen_asr ()
{
    outb1 (IR_ASR);
}

// Logically shift right the secondary
// register the number of times in the
// primary register.  Result in primary.
gen_lsr ()
{
    outb1 (IR_LSR);
}

// Arithmetic shift left secondary
// register the number of times in the
// primary register.  Result in primary.
gen_asl ()
{
    outb1 (IR_ASL);
}

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

gen_eq ()
{
    outb1 (IR_EQ);
}

gen_neq ()
{
    outb1 (IR_NE);
}

gen_lt ()
{
    outb1 (IR_LT);
}

gen_lte ()
{
    outb1 (IR_LTE);
}

gen_gt ()
{
    outb1 (IR_GT);
}

gen_gte ()
{
    outb1 (IR_GTE);
}

gen_ult ()
{
    outb1 (IR_ULT);
}

gen_ulte ()
{
    outb1 (IR_ULTE);
}

gen_ugt ()
{
    outb1 (IR_UGT);
}

gen_ugte ()
{
    outb1 (IR_UGTE);
}

/////////////////
/// TOP-LEVEL ///
/////////////////

char *
inclib ()
{
#ifdef  cpm
    return "B:";
#endif
#ifdef  unix
#ifdef  INCDIR
    return INCDIR;
#else
    return "";
#endif
#endif
}

int
assemble (char *s)
{
#ifdef  ASNM
    char buf[100];
    strcpy (buf, ASNM);
    strcat (buf, " ");
    strcat (buf, s);
    buf[strlen (buf) - 1] = 's';
    return system (buf);
#else
    return 0;
#endif
}

int
link ()
{
#ifdef  LDNM
    fputs ("I don't know how to link files yet\n", stderr);
#else
    return 0;
#endif
}
