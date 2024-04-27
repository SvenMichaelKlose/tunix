#include <stdio.h>
#include <error.h>

#include "defs.h"
#include "data.h"
#include "io.h"
#include "sym.h"
#include "lex.h"
#include "primary.h"
#include "gen.h"
#include "ir.h"
#include "sym.h"
#include "expr.h"

// Unsigned operand?
int
nosign (LVALUE * is)
{
    SYMBOL *ptr;
    return is->ptr_type
           || ((ptr = is->symbol)
               && (ptr->type & UNSIGNED));
}

int
rvalue_on_fetch (LVALUE * lval, int k)
{
    return k & FETCH ?
        rvalue (lval, k) :
        k;
}

int
push_and_rvalue_on_fetch (int k,
                          LVALUE * lval,
                          int (*hier) (LVALUE *))
{
    gen_push (k);
    return rvalue_on_fetch (lval, hier (lval));
}

void
binary (int k,
        LVALUE * lval,
        int (*hier) (LVALUE *),
        void (*gen) (void))
{
    push_and_rvalue_on_fetch (k, lval, hier);
    gen ();
    blanks ();
}

// lval.symbol - symbol table address,
// else 0 for constant lval.indirect -
// type indirect object to fetch, else
// 0 for static object lval.ptr_type -
// type pointer or array, else 0.
void
expression (int comma)
{
    LVALUE lval;
    do {
        rvalue_on_fetch (&lval, hier1 (&lval));
        if (!comma)
            return;
    } while (match (","));
}

// Assignment
int
hier1 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];
    char fc;
    k = hier1a (lval);
    if (match ("=")) {
        if (!(k & FETCH)) {
            needlval ();
            return 0;
        }
        if (lval->indirect)
            gen_push (k);
        rvalue_on_fetch (lval2, hier1 (lval2));
        store (lval);
        return 0;
    }
    fc = ch ();
    if (match ("-=")
        || match ("+=")
        || match ("*=")
        || match ("/=")
        || match ("%=")
        || match (">>=")
        || match ("<<=")
        || match ("&=")
        || match ("^=")
        || match ("|="))
        return hier1d (fc, lval, k);
   return k;
}

// ? : expression
int
hier1a (LVALUE * lval)
{
    int k, lab1, lab2;
    LVALUE lval2[1];

    k = hier1b (lval);
    blanks ();
    if (ch () != '?')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (!match ("?"))
            return 0;
        gen_test_jump (lab1 = getlabel (), FALSE);
        rvalue_on_fetch (lval2, hier1b (lval2));
        gen_jump (lab2 = getlabel ());
        def_local (lab1);
        blanks ();
        if (!match (":")) {
            perror ("missing colon");
            return 0;
        }
        rvalue_on_fetch (lval2, hier1b (lval2));
        def_local (lab2);
    }
}

// "||"
int
hier1b (LVALUE * lval)
{
    int k2, lab;
    LVALUE lval2[1];

    k2 = hier1c (lval);
    blanks ();
    if (!sstreq ("||"))
        return k2;
    rvalue_on_fetch (lval, k2);
    FOREVER {
        if (!match ("||"))
            return 0;
        gen_test_jump (lab = getlabel (), TRUE);
        rvalue_on_fetch (lval2, hier1c (lval2));
        def_local (lab);
        gen_tobool ();
    }
}

// "&&"
int
hier1c (LVALUE * lval)
{
    int k2, lab;
    LVALUE lval2[1];

    k2 = hier2 (lval);
    blanks ();
    if (!sstreq ("&&"))
        return k2;
    rvalue_on_fetch (lval, k2);
    FOREVER {
        if (!match ("&&"))
            return 0;
        gen_test_jump (lab = getlabel (), FALSE);
        rvalue_on_fetch (lval2, hier2 (lval2));
        def_local (lab);
        gen_tobool ();
    }
}

int
hier1d (char fc, LVALUE * lval, int k)
{
    LVALUE lval2[1];
    if (!(k & FETCH)) {
        needlval ();
        return 0;
    }
    if (lval->indirect)
        gen_push (k);
    k = rvalue (lval, k);
    push_and_rvalue_on_fetch (k, lval2, hier1);
    switch (fc) {
    case '-':
        if (dbltest (lval, lval2))
            gen_mul_const (lval->ptr_type,
                           lval->tagsym ?
                                lval->tagsym->size :
                                INTSIZE);
        gen_sub ();
        result (lval, lval2);
        break;
    case '+':
        if (dbltest (lval, lval2))
            gen_mul_const (lval->ptr_type,
                           lval->tagsym ?
                               lval->tagsym->size :
                               INTSIZE);
        gen_add (lval, lval2);
        result (lval, lval2);
        break;
    case '*':
        gen_mul ();
        break;
    case '/':
        gen_divide (lval, lval2);
    case '%':
        gen_modulo (lval, lval2);
        break;
    case '>':
        gen_ashiftr (lval);
        break;
    case '<':
        gen_asl ();
        break;
    case '&':
        gen_and ();
        break;
    case '^':
        gen_xor ();
        break;
    case '|':
        gen_or ();
        break;
    }
    store (lval);
    return 0;
}
 
// "|"
int
hier2 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier3 (lval);
    blanks ();
    if (ch () != '|'
        || nch () == '|'
        || nch () == '=')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (ch () == '|'
            && nch () != '|'
            && nch () != '=') {
            inbyte ();
            binary (k, lval2, hier3, gen_or);
        } else
            return 0;
    }
}

// "^"
int
hier3 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier4 (lval);
    blanks ();
    if (ch () != '^'
        || nch () == '=')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (ch () == '^'
            && nch () != '=') {
            inbyte ();
            binary (k, lval2, hier4, gen_xor);
        } else
            return 0;
    }
}

// "&"
int
hier4 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier5 (lval);
    blanks ();
    if (ch () != '&'
        || nch () == '|'
        || nch () == '=')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (ch () == '&'
            && nch () != '&'
            && nch () != '=') {
            inbyte ();
            binary (k, lval2, hier5, gen_and);
        } else
            return 0;
    }

}

void hier6rvalue (int k, LVALUE * lval);

// "==" and "!="
int
hier5 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier6 (lval);
    blanks ();
    if (!sstreq ("==") & !sstreq ("!="))
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (match ("==")) {
            hier6rvalue (k, lval2);
            gen_eq ();
        } else if (match ("!=")) {
            hier6rvalue (k, lval2);
            gen_neq ();
        } else
            return 0;
    }

}

void
hier6rvalue (int k, LVALUE * lval)
{
    gen_push (k);
    rvalue_on_fetch (lval, hier6 (lval));
}

// Comparison other than "==" and "!=".
int
hier6 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier7 (lval);
    blanks ();
    if (!sstreq ("<") &&
        !sstreq ("<=") && !sstreq (">=") && !sstreq (">"))
        return k;
    if (sstreq ("<<") || sstreq (">>"))
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (match ("<=")) {
            binary (k, lval2, hier7,
                    nosign (lval) || nosign (lval2) ?
                        gen_ulte :
                        gen_lte);
        } else if (match (">=")) {
            binary (k, lval2, hier7,
                    nosign (lval) || nosign (lval2) ?
                        gen_ugte :
                        gen_gte);
        } else if (sstreq ("<")
                   && !sstreq ("<<")) {
            inbyte ();
            binary (k, lval2, hier7,
                    nosign (lval) || nosign (lval2) ?
                        gen_ult :
                        gen_lt);
        } else if (sstreq (">")
                   && !sstreq (">>")) {
            inbyte ();
            binary (k, lval2, hier7,
                    nosign (lval) || nosign (lval2) ?
                        gen_ugt :
                        gen_gt);
        } else
            return 0;
    }
}

// "<<" and ">>"
int
hier7 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier8 (lval);
    blanks ();
    if ((!sstreq (">>") && !sstreq ("<<"))
        || sstreq (">>=") || sstreq ("<<="))
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (sstreq (">>") && !sstreq (">>=")) {
            inbyte ();
            inbyte ();
            binary (k, lval2, hier8,
                    nosign (lval) ?
                        gen_lsr :
                        gen_asr);
        } else if (sstreq ("<<") && !sstreq ("<<=")) {
            inbyte ();
            inbyte ();
            binary (k, lval2, hier8, gen_asl);
        } else
            return 0;
    }
}

// "+" and "-"
int
hier8 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier9 (lval);
    blanks ();
    if ((ch () != '+' && ch () != '-')
        || nch () == '=')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (match ("+")) {
            push_and_rvalue_on_fetch (k, lval2, hier9);
            // if left is pointer and right is int, scale right 
            if (dbltest (lval, lval2))
                gen_mul_const (lval->ptr_type,
                              lval->tagsym ? lval->tagsym->
                              size : INTSIZE);
            // will scale left if right int pointer and left int 
            gen_add (lval, lval2);
            result (lval, lval2);
        } else if (match ("-")) {
            push_and_rvalue_on_fetch (k, lval2, hier9);
            /* if dbl, can only be: pointer - int, or
               pointer - pointer, thus,
               in first case, int is scaled up,
               in second, result is scaled down. */
            if (dbltest (lval, lval2))
                gen_mul_const (lval->ptr_type,
                              lval->tagsym ? lval->tagsym->
                              size : INTSIZE);
            gen_sub ();
            // if both pointers, scale result 
            if (lval->ptr_type & CINT
                && lval2->ptr_type & CINT)
                gen_div2 ();   // divide by intsize 
            result (lval, lval2);
        } else
            return 0;
    }
}

// "*", "/" and "%"
int
hier9 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier10 (lval);
    blanks ();
    if ((ch () != '*'
         && ch () != '/'
         && (ch () != '%'))
        || nch () == '=')
        return k;
    k = rvalue_on_fetch (lval, k);
    FOREVER {
        if (match ("*")) {
            binary (k, lval2, hier10, gen_mul);
        } else if (match ("/")) {
            binary (k, lval2, hier10,
                    nosign (lval) || nosign (lval2) ?
                        gen_udiv :
                        gen_div);
        } else if (match ("%")) {
            binary (k, lval2, hier10,
                    nosign (lval) || nosign (lval2) ?
                        gen_umod :
                        gen_mod);
        } else
            return 0;
    }
}

// "++", "--" and unary "-"
int
hier10 (LVALUE * lval)
{
    int k;
    SYMBOL *ptr;

    if (match ("++")) {
        if (!((k = hier10 (lval)) & FETCH)) {
            needlval ();
            return 0;
        }
        if (lval->indirect)
            gen_push (k);
        rvalue (lval, k);
        gen_ptrinc (lval);
        store (lval);
        return REGA;
    } else if (match ("--")) {
        if (!((k = hier10 (lval)) & FETCH)) {
            needlval ();
            return 0;
        }
        if (lval->indirect)
            gen_push (k);
        rvalue (lval, k);
        gen_ptrdec (lval);
        store (lval);
        return REGA;
    } else if (match ("-")) {
        rvalue_on_fetch (lval, hier10 (lval));
        gen_neg ();
        return REGA;
    } else if (match ("~")) {
        rvalue_on_fetch (lval, hier10 (lval));
        gen_complement ();
        return REGA;
    } else if (match ("!")) {
        rvalue_on_fetch (lval, hier10 (lval));
        gen_not ();
        return REGA;
    } else if (ch () == '*' && nch () != '=') {
        inbyte ();
        k = rvalue_on_fetch (lval, hier10 (lval));
        if ((ptr = lval->symbol))
            lval->indirect = ptr->type;
        else
            lval->indirect = CINT;
        // Not pointer or array.
        lval->ptr_type = 0;
        return FETCH | k;
    } else if (ch () == '&' && nch () != '&'
               && nch () != '=') {
        inbyte ();
        k = hier10 (lval);
        if (!(k & FETCH)) {
            // Without this check, this
            // error triggers when
            // trying to evaluate a
            // struct's address (a legal
            // operation).  Because
            // structs are stored as an
            // address, nothing more
            // than not erroring is
            // needed to load their
            // address.
            if (lval->symbol->type != STRUCT)
                perror ("illegal address");
            return 0;
        }
        ptr = lval->symbol;
        lval->ptr_type = ptr->type;
        if (lval->indirect) {
            if (k & REGB)
                gen_swap ();
            return REGA;
        }
        // Global and non-array.
        gen_ldacig ((ptr = lval->symbol)->name);
        lval->indirect = ptr->type;
        return REGA;
    } else {
        k = hier11 (lval);
        if (match ("++")) {
            if (!(k & FETCH)) {
                needlval ();
                return 0;
            }
            if (lval->indirect)
                gen_push (k);
            rvalue (lval, k);
            gen_ptrinc (lval);
            store (lval);
            gen_ptrdec (lval);
            return REGA;
        } else if (match ("--")) {
            if (!(k & FETCH)) {
                needlval ();
                return 0;
            }
            if (lval->indirect)
                gen_push (k);
            rvalue (lval, k);
            gen_ptrdec (lval);
            store (lval);
            gen_ptrinc (lval);
            return REGA;
        } else
            return k;
    }
}

// Array subscript, function,
// direct/indirect member
int
hier11 (LVALUE * lval)
{
    int direct, k;
    SYMBOL *symbol;
    char sname[NAMESIZE];

    k = primary (lval);
    symbol = lval->symbol;
    blanks ();
    if (ch () == '['
        || ch () == '('
        || ch () == '.'
        || (ch () == '-'
            && nch () == '>'))
        FOREVER {
            if (match ("[")) {
                if (!symbol) {
                    junk ();
                    needbrack ("]");
                    perror ("can't subscript");
                    return 0;
                } else if (symbol->identity == POINTER) {
                    k = rvalue (lval, k);
                } else if (symbol->identity != ARRAY) {
                    perror ("can't subscript");
                    k = 0;
                }
                gen_push (k);
                expression (YES);
                needbrack ("]");
                gen_mul_const (symbol->type,
                              tags[symbol->tag].size);
                gen_add (NULL, NULL);
                lval->indirect = symbol->type;
                lval->ptr_type = 0;
                return FETCH | REGA;
            } else if (match ("(")) {
                if (!symbol)
                    callfunction (0);
                else if (symbol->identity != FUNCTION) {
                    rvalue (lval, k);
                    callfunction (0);
                } else
                    callfunction (symbol->name);
                lval->symbol = 0;
                return 0;
            } else if ((direct = match ("."))
                       || match ("->")) {
                if (!lval->tagsym) {
                    perror ("can't take member");
                    junk ();
                    return 0;
                }
                if (!symname (sname) ||
                    (!(symbol = find_member (lval->tagsym, sname)))) {
                    perror ("unknown member");
                    junk ();
                    return 0;
                }
                if ((k & FETCH) && !direct)
                    k = rvalue (lval, k);
                if (k == REGB)
                    gen_swap ();

                // move pointer from struct begin to struct member 
                gen_add_const (symbol->offset);
                lval->symbol = symbol;
                lval->indirect = symbol->type;
                lval->ptr_type = 0;
                lval->tagsym = NULL_TAG;
                if (symbol->type == STRUCT)
                    lval->tagsym = &tags[symbol->tag];
                if (symbol->identity == POINTER) {
                    lval->indirect = CINT;
                    lval->ptr_type = symbol->type;
                }
                if (symbol->identity == ARRAY
                    || (symbol->type == STRUCT
                        && symbol->identity == VARIABLE)) {
                    lval->ptr_type = symbol->type;
                    k = 0;
                } else
                    k = FETCH | REGA;
            } else
                return k;
        }
    if (!symbol)
        return k;
    if (symbol->identity == FUNCTION) {
        gen_ldacig (symbol->name);
        return 0;
    }
    return k;
}
