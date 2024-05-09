
//      File code8080.c: 2.2 (84/08/31,10:05:09) 

/*% cc -O -c %
 *
 */

#include <stdio.h>
#include "defs.h"
#include "data.h"

/*      Define ASNM and LDNM to the names of the assembler and linker
        respectively */

/*
 *      Some predefinitions:
 *
 *      INTSIZE is the size of an integer in the target machine
 *      BYTEOFF is the offset of an byte within an integer on the
 *              target machine. (ie: 8080,pdp11 = 0, 6809 = 1,
 *              360 = 3)
 *      This compiler assumes that an integer is the SAME length as
 *      a pointer - in fact, the compiler uses INTSIZE for both.
 */

/**
 * print all assembler info before any code is generated
 */
void
header ()
{
    outs
        ("; Small C 8080\n;\tCoder (2.4,84/11/27)\n;");
    frontend_version ();
    newline ();
    outl
        ("\t;program area SMALLC_GENERATED is RELOCATABLE");
    outl ("\t.module SMALLC_GENERATED");
    outl
        ("\t.list   (err, loc, bin, eqt, cyc, lin, src, lst, md)");
    outl ("\t.nlist  (pag)");
}

/**
 * prints new line
 * @return
 */
newline ()
{
#if __CYGWIN__ == 1
    outb (CR);
#endif
    outb (LF);
}

void
initmac ()
{
    defmac ("cpm\t1");
    defmac ("I8080\t1");
    defmac ("RMAC\t1");
    defmac ("smallc\t1");
}

/**
 * Output internal generated label prefix
 */
void
output_label_prefix ()
{
    outb ('$');
}

/**
 * Output a label definition terminator
 */
void
output_label_terminator ()
{
    outb (':');
}

/**
 * begin a comment line for the assembler
 */
void
gen_comment ()
{
    outb (';');
}

/**
 * print any assembler stuff needed after all code
 */
void
trailer ()
{
    outl (";\t.end");
}

/**
 * text (code) segment
 */
void
gen_code_segment ()
{
    outl
        ("\t.area  SMALLC_GENERATED  (REL,CON,CSEG)");
}

/**
 * data segment
 */
void
gen_data_segment ()
{
    outl
        ("\t.area  SMALLC_GENERATED_DATA  (REL,CON,DSEG)");
}

/**
 * Output the variable symbol at scptr as an extrn or a public
 * @param scptr
 */
void
gen_decl_var (SYMBOL * scptr)
{
    if (symbol_table[current_symbol_table_idx].storage ==
        STATIC)
        return;
    outtabs (scptr->storage ==
                     EXTERN ? ";extrn\t" : ".globl\t");
    outs (scptr->name);
    newline ();
}

/**
 * Output the function symbol at scptr as an extrn or a public
 * @param scptr
 */
void
gen_decl_fun (SYMBOL * scptr)
{
    if (scptr->storage == STATIC)
        return;
    outtabs (scptr->offset ==
                     FUNCTION ? ".globl\t" : ";extrn\t");
    outs (scptr->name);
    newline ();
}

/**
 * Output a decimal number to the assembler file, with # prefix
 * @param num
 */
void
outn (num)
int num;
{
    outb ('#');
    outn (num);
}

/**
 * fetch a static memory cell into the primary register
 * @param sym
 */
void
gen_get_memory (SYMBOL * sym)
{
    if ((sym->identity != POINTER) && (sym->type == CCHAR)) {
        outtabs ("lda\t");
        outs (sym->name);
        newline ();
        gen_call ("ccsxt");
    } else if ((sym->identity != POINTER)
               && (sym->type == UCHAR)) {
        outtabs ("lda\t");
        outs (sym->name);
        newline ();
        outl ("mov \tl,a");
        outl ("mvi \th,#0");
    } else {
        outtabs ("lhld\t");
        outs (sym->name);
        newline ();
    }
}

/**
 * asm - fetch the address of the specified symbol into the primary register
 * @param sym the symbol name
 * @return which register pair contains result
 */
int
gen_get_local (SYMBOL * sym)
{
    if (sym->storage == LSTATIC) {
        gen_load_1st ();
        print_label (sym->offset);
        newline ();
        return REGA;
    } else {
        if (uflag && !(sym->identity == ARRAY)) {       /* ||
                                                           (sym->identity == VARIABLE && sym->type == STRUCT))) { */
            outtabs ("ldsi\t");
            outn (sym->offset - stkp);
            newline ();
            return REGB;
        } else {
            gen_load_1st ();
            outn (sym->offset - stkp);
            newline ();
            outl ("dad \tsp");
            return REGA;
        }
    }
}

/**
 * asm - store the primary register into the specified static memory cell
 * @param sym
 */
void
gen_put_memory (SYMBOL * sym)
{
    if ((sym->identity != POINTER) && (sym->type & CCHAR)) {
        outl ("mov \ta,l");
        outtabs ("sta \t");
    } else {
        outtabs ("shld\t");
    }
    outs (sym->name);
    newline ();
}

/**
 * store the specified object type in the primary register
 * at the address in secondary register (on the top of the stack)
 * @param typeobj
 */
void
gen_put_indirect (char typeobj)
{
    gen_pop ();
    if (typeobj & CCHAR) {
        //gen_call("ccpchar"); 
        outl ("mov \ta,l");
        outl ("stax\td");
    } else {
        if (uflag) {
            outl ("shlx");
        } else {
            gen_call ("ccpint");
        }
    }
}

/**
 * fetch the specified object type indirect through the primary
 * register into the primary register
 * @param typeobj object type
 */
void
gen_get_indirect (char typeobj, int reg)
{
    if (typeobj == CCHAR) {
        if (reg & REGB) {
            gen_swap ();
        }
        gen_call ("ccgchar");
    } else if (typeobj == UCHAR) {
        if (reg & REGB) {
            gen_swap ();
        }
        //gen_call("cguchar"); 
        outl ("mov \tl,m");
        outl ("mvi \th,0");
    } else {                    //int 
        if (uflag) {
            if (reg & REGA) {
                gen_swap ();
            }
            outl ("lhlx");
        } else {
            gen_call ("ccgint");
        }
    }
}

/**
 * swap the primary and secondary registers
 */
gen_swap ()
{
    outl ("xchg");
}

/**
 * print partial instruction to get an immediate value into
 * the primary register
 */
gen_load_1st ()
{
    outtabs ("lxi \th,");
}

/**
 * push the primary register onto the stack
 */
gen_push (int reg)
{
    if (reg & REGB) {
        outl ("push\td");
        stkp = stkp - INTSIZE;
    } else {
        outl ("push\th");
        stkp = stkp - INTSIZE;
    }
}

/**
 * pop the top of the stack into the secondary register
 */
gen_pop ()
{
    outl ("pop \td");
    stkp = stkp + INTSIZE;
}

/**
 * swap the primary register and the top of the stack
 */
gen_swap_stack ()
{
    outl ("xthl");
}

/**
 * call the specified subroutine name
 * @param sname subroutine name
 */
gen_call (char *sname)
{
    outtabs ("call\t");
    outs (sname);
    newline ();
}

/**
 * declare entry point
 */
declare_entry_point (char *symbol_name)
{
    outs (symbol_name);
    output_label_terminator ();
    //newline(); 
}

/**
 * return from subroutine
 */
gen_ret ()
{
    outl ("ret");
}

/**
 * perform subroutine call to value on top of stack
 */
callstk ()
{
    gen_load_1st ();
    outs ("#.+5");
    newline ();
    gen_swap_stack ();
    outl ("pchl");
    stkp = stkp + INTSIZE;
}

/**
 * jump to specified internal label number
 * @param label the label
 */
gen_jump (label)
int label;
{
    outtabs ("jmp \t");
    print_label (label);
    newline ();
}

/**
 * test the primary register and jump if false to label
 * @param label the label
 * @param ft if true jnz is generated, jz otherwise
 */
gen_test_jump (label, ft)
int label, ft;
{
    outl ("mov \ta,h");
    outl ("ora \tl");
    if (ft)
        outtabs ("jnz \t");
    else
        outtabs ("jz  \t");
    print_label (label);
    newline ();
}

/**
 * print pseudo-op  to define a byte
 */
gen_datab ()
{
    outtabs (".db\t");
}

/**
 * print pseudo-op to define storage
 */
gen_bss ()
{
    outtabs (".ds\t");
}

/**
 * print pseudo-op to define a word
 */
gen_dataw ()
{
    outtabs (".dw\t");
}

/**
 * modify the stack pointer to the new value indicated
 * @param newstkp new value
 */
gen_modify_stack (int newstkp)
{
    int k;

    k = newstkp - stkp;
    if (k == 0)
        return (newstkp);
    if (k > 0) {
        if (k < 7) {
            if (k & 1) {
                outl ("inx \tsp");
                k--;
            }
            while (k) {
                outl ("pop \tb");
                k = k - INTSIZE;
            }
            return (newstkp);
        }
    } else {
        if (k > -7) {
            if (k & 1) {
                outl ("dcx \tsp");
                k++;
            }
            while (k) {
                outl ("push\tb");
                k = k + INTSIZE;
            }
            return (newstkp);
        }
    }
    gen_swap ();
    gen_load_1st ();
    outn (k);
    newline ();
    outl ("dad \tsp");
    outl ("sphl");
    gen_swap ();
    return (newstkp);
}

/**
 * multiply the primary register by INTSIZE
 */
gen_mul2 ()
{
    outl ("dad \th");
}

/**
 * divide the primary register by INTSIZE, never used
 */
gen_div2 ()
{
    gen_push (REGA);          // push primary in prep for gasr 
    gen_load_1st ();
    outn (1);
    newline ();
    gen_asr ();  // divide by two 
}

/**
 * Case jump instruction
 */
gen_jump_case ()
{
    outtabs ("jmp \tcccase");
    newline ();
}

/**
 * add the primary and secondary registers
 * if lval2 is int pointer and lval is not, scale lval
 * @param lval
 * @param lval2
 */
gen_add (lval, lval2)
int *lval, *lval2;
{
    gen_pop ();
    if (dbltest (lval2, lval)) {
        gen_swap ();
        gen_mul2 ();
        gen_swap ();
    }
    outl ("dad \td");
}

/**
 * subtract the primary register from the secondary
 */
gen_sub ()
{
    gen_pop ();
    gen_call ("ccsub");
}

/**
 * multiply the primary and secondary registers (result in primary)
 */
gen_mul ()
{
    gen_pop ();
    gen_call ("ccmul");
}

/**
 * divide the secondary register by the primary
 * (quotient in primary, remainder in secondary)
 */
gen_div ()
{
    gen_pop ();
    gen_call ("ccdiv");
}

/**
 * unsigned divide the secondary register by the primary
 * (quotient in primary, remainder in secondary)
 */
gen_udiv ()
{
    gen_pop ();
    gen_call ("ccudiv");
}

/**
 * compute the remainder (mod) of the secondary register
 * divided by the primary register
 * (remainder in primary, quotient in secondary)
 */
gen_mod ()
{
    gen_div ();
    gen_swap ();
}

/**
 * compute the remainder (mod) of the secondary register
 * divided by the primary register
 * (remainder in primary, quotient in secondary)
 */
gen_umod ()
{
    gen_udiv ();
    gen_swap ();
}

/**
 * inclusive 'or' the primary and secondary registers
 */
gen_or ()
{
    gen_pop ();
    gen_call ("ccor");
}

/**
 * exclusive 'or' the primary and secondary registers
 */
gen_xor ()
{
    gen_pop ();
    gen_call ("ccxor");
}

/**
 * 'and' the primary and secondary registers
 */
gen_and ()
{
    gen_pop ();
    gen_call ("ccand");
}

/**
 * arithmetic shift right the secondary register the number of
 * times in the primary register (results in primary register)
 */
gen_asr ()
{
    gen_pop ();
    gen_call ("ccasr");
}

/**
 * logically shift right the secondary register the number of
 * times in the primary register (results in primary register)
 */
gen_lsr ()
{
    gen_pop ();
    gen_call ("cclsr");
}

/**
 * arithmetic shift left the secondary register the number of
 * times in the primary register (results in primary register)
 */
gen_asl ()
{
    gen_pop ();
    gen_call ("ccasl");
}

/**
 * two's complement of primary register
 */
gen_neg ()
{
    gen_call ("ccneg");
}

/**
 * logical complement of primary register
 */
gen_not ()
{
    gen_call ("cclneg");
}

/**
 * one's complement of primary register
 */
gen_complement ()
{
    gen_call ("cccom");
}

/**
 * Convert primary value into logical value (0 if 0, 1 otherwise)
 */
gen_tobool ()
{
    gen_call ("ccbool");
}

/**
 * increment the primary register by 1 if char, INTSIZE if int
 */
gen_ptrinc (LVALUE * lval)
{
    switch (lval->ptr_type) {
    case STRUCT:
        gen_load_1st2 ();
        outn (lval->tagsym->size);
        newline ();
        outl ("dad \td");
        break;
    case CINT:
    case UINT:
        outl ("inx \th");
    default:
        outl ("inx \th");
        break;
    }
}

/**
 * decrement the primary register by one if char, INTSIZE if int
 */
gen_ptrdec (LVALUE * lval)
{
    outl ("dcx \th");
    switch (lval->ptr_type) {
    case CINT:
    case UINT:
        outl ("dcx \th");
        break;
    case STRUCT:
        gen_load_1st2 ();
        outn (lval->tagsym->size - 1);
        newline ();
        // two's complement 
        outl ("mov  \ta,d");
        outl ("cma");
        outl ("mov  \td,a");
        outl ("mov  \ta,e");
        outl ("cma");
        outl ("mov \te,a");
        outl ("inx \td");
        // subtract 
        outl ("dad \td");
        break;
    default:
        break;
    }
}

/**
 * following are the conditional operators.
 * they compare the secondary register against the primary register
 * and put a literal 1 in the primary if the condition is true,
 * otherwise they clear the primary register
 */

/**
 * equal
 */
gen_eq ()
{
    gen_pop ();
    gen_call ("cceq");
}

/**
 * not equal
 */
gen_neq ()
{
    gen_pop ();
    gen_call ("ccne");
}

/**
 * less than (signed)
 */
gen_lt ()
{
    gen_pop ();
    gen_call ("cclt");
}

/**
 * less than or equal (signed)
 */
gen_lte ()
{
    gen_pop ();
    gen_call ("ccle");
}

/**
 * greater than (signed)
 */
gen_gt ()
{
    gen_pop ();
    gen_call ("ccgt");
}

/**
 * greater than or equal (signed)
 */
gen_gte ()
{
    gen_pop ();
    gen_call ("ccge");
}

/**
 * less than (unsigned)
 */
gen_ult ()
{
    gen_pop ();
    gen_call ("ccult");
}

/**
 * less than or equal (unsigned)
 */
gen_ulte ()
{
    gen_pop ();
    gen_call ("ccule");
}

/**
 * greater than (unsigned)
 */
gen_ugt ()
{
    gen_pop ();
    gen_call ("ccugt");
}

/**
 * greater than or equal (unsigned)
 */
gen_ugte ()
{
    gen_pop ();
    gen_call ("ccuge");
}

char *
inclib ()
{
#ifdef  cpm
    return ("B:");
#endif
#ifdef  unix
#ifdef  INCDIR
    return (INCDIR);
#else
    return "";
#endif
#endif
}

/**
 * Squirrel away argument count in a register that modstk doesn't touch.
 * @param d
 */
gnargs (d)
int d;
{
    outtabs ("mvi \ta,");
    outn (d);
    newline ();
}

int
assemble (s)
char *s;
{
#ifdef  ASNM
    char buf[100];
    strcpy (buf, ASNM);
    strcat (buf, " ");
    strcat (buf, s);
    buf[strlen (buf) - 1] = 's';
    return (system (buf));
#else
    return (0);
#endif

}

int
link ()
{
#ifdef  LDNM
    fputs ("I don't know how to link files yet\n", stderr);
#else
    return (0);
#endif
}

/**
 * print partial instruction to get an immediate value into
 * the secondary register
 */
gen_load_1st2 ()
{
    outtabs ("lxi \td,");
}

/**
 * add offset to primary register
 * @param val the value
 */
gen_add_const (int val)
{
    gen_load_1st2 ();
    outn (val);
    newline ();
    outl ("dad \td");
}

/**
 * multiply the primary register by the length of some variable
 * @param type
 * @param size
 */
gen_mul_const (int type, int size)
{
    switch (type) {
    case CINT:
    case UINT:
        gen_mul2 ();
        break;
    case STRUCT:
        gen_load_1st2 ();
        outn (size);
        newline ();
        gen_call ("ccmul");
        break;
    default:
        break;
    }
}
