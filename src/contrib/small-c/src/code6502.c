// Becoming an IR code generator.

#include <stdio.h>
#include "defs.h"
#include "data.h"

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

newline ()
{
#if __CYGWIN__ == 1
    output_byte (CR);
#endif
    output_byte (LF);
}

output_number (int num)
{
    output_decimal (num);
}

output_label_prefix ()
{
    output_byte ('_');
}

output_label_terminator ()
{
    output_byte (':');
    newline ();
}

gen_comment ()
{
    output_string ("; ");
}

////////////////
/// SEGMENTS ///
////////////////

gen_code_segment ()
{
    output_line (".code");
}

gen_data_segment ()
{
    output_line (".data");
}

////////////
/// DATA ///
////////////

gen_datab ()
{
    output_with_tab (".byte ");
}

gen_dataw ()
{
    output_with_tab (".word ");
}

gen_bss ()
{
    output_with_tab (".res ");
}

//////////////
/// LABELS ///
//////////////

def_local (int nlab)
{
    output_decimal (nlab);
    output_label_terminator ();
}

def_global (char *n)
{
    output_string (n);
    output_label_terminator ();
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

_gen_storage_decl (int do_import, char *n)
{
    output_with_tab (
        do_import ?
            ".import " :
            ".export "
    );
    output_string (n);
    newline ();
}

// Import/export variable symbol.
gen_decl_var (SYMBOL * scptr)
{
    if (symbol_table[current_symbol_table_idx].storage == STATIC)
        return;
    _gen_storage_decl (scptr->storage == EXTERN, scptr->name);
}

// Import/export function symbol.
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
    gen_call ("xchg");
}

gen_load_1st ()
{
    output_with_tab ("lhli ");
}

_gen_load_2nd_const (int v)
{
    output_with_tab ("ldei ");
    output_number (v);
    newline ();
}

gen_get_local (SYMBOL * sym)
{
    if (sym->storage == LSTATIC) {
        gen_load_1st ();
        gen_local (sym->offset);
        newline ();
    } else {
        gen_load_1st ();
        output_number (sym->offset - stkp);
        newline ();
        gen_call ("add_sp");
    }
    return HL_REG;
}

// Pointer increment
gen_ptrinc (LVALUE * lval)
{
    switch (lval->ptr_type) {
    case STRUCT:
        _gen_load_2nd_const (lval->tagsym->size);
        gen_call ("add_de");
        break;
    case CINT:
    case UINT:
    default:
        gen_call ("inc_hl");
        break;
    }
}

// Pointer decrement
gen_ptrdec (LVALUE * lval)
{
    gen_call ("dec_hl");
    switch (lval->ptr_type) {
    case CINT:
    case UINT:
        gen_call ("dec_hl");
        break;
    case STRUCT:
        _gen_load_2nd_const (lval->tagsym->size - 1);
        // two's complement of secondary.
        output_line ("mov   a,d");
        output_line ("cma");
        output_line ("mov   d,a");
        output_line ("mov   a,e");
        output_line ("cma");
        output_line ("mov  e,a");
        output_line ("inx  d");
        // subtract 
        gen_call ("add_de");
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
        output_with_tab ("lda ");
        output_string (sym->name);
        newline ();
        gen_call ("ccsxt");
    } else if (sym->identity != POINTER
               && sym->type == UCHAR) {
        output_with_tab ("lda ");
        output_string (sym->name);
        newline ();
        output_line ("sta l");
        output_line ("lda #0");
        output_line ("sta h");
    } else {
        output_with_tab ("lhl ");
        output_string (sym->name);
        newline ();
    }
}

void
gen_put_memory (SYMBOL * sym)
{
    if (sym->identity != POINTER
        && (sym->type & CCHAR)) {
        output_line ("lda l");
        output_with_tab ("sta ");
    } else
        output_with_tab ("sthl ");
    output_string (sym->name);
    newline ();
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
    if (typeobj & CCHAR)
        gen_call ("ccpchar"); 
    else
        gen_call ("ccpint");
}

// Fetch the specified object type
// indirect through the primary register
// into the primary register.
void
gen_get_indirect (char typeobj, int reg)
{
    if (typeobj == CCHAR) {
        if (reg & DE_REG)
            gen_swap ();
        gen_call ("ccgchar");
    } else if (typeobj == UCHAR) {
        if (reg & DE_REG)
            gen_swap ();
        gen_call ("cguchar");
    } else
        gen_call ("ccgint");
}

/////////////
/// STACK ///
/////////////

gen_push (int reg)
{
    if (reg & DE_REG) {
        output_line ("push de");
        stkp = stkp - INTSIZE;
    } else {
        output_line ("push hl");
        stkp = stkp - INTSIZE;
    }
}

// Pop into secondary.
gen_pop ()
{
    output_line ("pop de");
    stkp = stkp + INTSIZE;
}

// Swap primary and top of stack.
gen_swap_stack ()
{
    output_line ("xthl");
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
                output_line ("tsx");
                output_line ("inx");
                output_line ("txs");
                k--;
            }
            while (k) {
                output_line ("pla");
                output_line ("sta b");
                k = k - INTSIZE;
            }
            return newstkp;
        }
    } else {
        if (k > -7) {
            if (k & 1) {
                output_line ("tsx");
                output_line ("dex");
                output_line ("txs");
                k++;
            }
            while (k) {
                output_line ("lda b");
                output_line ("pha");
                k = k + INTSIZE;
            }
            return newstkp;
        }
    }
    gen_swap ();
    gen_load_1st ();
    output_number (k);
    newline ();
    gen_call ("add_sp");
    output_line ("sphl");
    gen_swap ();
    return newstkp;
}

/////////////////
/// FUNCTIONS ///
/////////////////

gen_call (char *sname)
{
    output_with_tab ("jsr ");
    output_string (sname);
    newline ();
}

// Call with argument in secondary popped
// from stack.
gen_call1 (char *n)
{
    gen_pop ();
    gen_call (n);
}

gen_ret ()
{
    output_line ("rts");
}

// Perform subroutine call to value on
// top of stack.
callstk ()
{
    gen_load_1st ();
    output_string ("#.+5");
    newline ();
    gen_swap_stack ();
    gen_call ("callptr");
    stkp = stkp + INTSIZE;
}

// Squirrel away argument count in a
// register that modstk doesn't touch.
gnargs (int d)
{
    output_with_tab (";#arg");
    output_number (d);
    newline ();
}

/////////////
/// JUMPS ///
/////////////

gen_jump (int label)
{
    output_with_tab ("jmp ");
    gen_local (label);
    newline ();
}

// 'case' jump
gen_jump_case ()
{
    output_with_tab ("jmp cccase");
    newline ();
}

// Test primary and jump to label.
// ft != 0: Jump if not zero.
// ft == 0: Jump if zero.
gen_test_jump (int label, int ft)
{
    output_line ("lda h");
    output_line ("ora l");
    if (ft)
        output_with_tab ("bne ");
    else
        output_with_tab ("beq ");
    gen_local (label);
    newline ();
}

/////////////
/// TRUTH ///
/////////////

gen_tobool ()
{
    gen_call ("ccbool");
}

gen_not ()
{
    gen_call ("cclneg");
}


///////////////////
/// ARITHMETICS ///
///////////////////

gen_neg ()
{
    gen_call ("ccneg");
}

gen_mul2 ()
{
    gen_call ("asl_hl");
}

gen_div2 ()
{
    // push primary in prep for
    // gen_asr ().
    gen_push (HL_REG);
    gen_load_1st ();
    output_number (1);
    newline ();
    gen_asr ();
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
    gen_call ("add_de");
}

// Add offset to primary.
gen_add_const (int val)
{
    _gen_load_2nd_const (val);
    gen_call ("add_de");
}

gen_sub ()
{
    gen_call1 ("ccsub");
}

// Multiply primary and secondary
// registers (result in primary)
gen_mul ()
{
    gen_call1 ("ccmul");
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
        gen_call ("ccmul");
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
    gen_call1 ("ccdiv");
}

// Unsigned divide secondary register
// by the primary.  Quotient in primary,
// remainder in secondary.
gen_udiv ()
{
    gen_call1 ("ccudiv");
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
    gen_call1 ("ccor");
}

// Exclusive 'or' the primary and
// secondary registers.
gen_xor ()
{
    gen_call1 ("ccxor");
}

// 'and' primary and secondary.
gen_and ()
{
    gen_call1 ("ccand");
}

// One's complement of primary.
gen_complement ()
{
    gen_call ("cccom");
}

//////////////
/// SHIFTS ///
//////////////

// Arithmetic shift right the secondary // register the number of times in the
// primary.  Result in primary.
gen_asr ()
{
    gen_call1 ("ccasr");
}

// Logically shift right the secondary
// register the number of times in the
// primary register.  Result in primary.
gen_lsr ()
{
    gen_call1 ("cclsr");
}

// Arithmetic shift left secondary
// register the number of times in the
// primary register.  Result in primary.
gen_asl ()
{
    gen_call1 ("ccasl");
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
    gen_call1 ("cceq");
}

gen_neq ()
{
    gen_call1 ("ccne");
}

gen_lt ()
{
    gen_call1 ("cclt");
}

gen_lte ()
{
    gen_call1 ("ccle");
}

gen_gt ()
{
    gen_call1 ("ccgt");
}

gen_gte ()
{
    gen_call1 ("ccge");
}

gen_ult ()
{
    gen_call1 ("ccult");
}

gen_ulte ()
{
    gen_call1 ("ccule");
}

gen_ugt ()
{
    gen_call1 ("ccugt");
}

gen_ugte ()
{
    gen_call1 ("ccuge");
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
