#include <stdio.h>
#include "defs.h"
#include "data.h"

// Unsigned operand?
nosign (LVALUE * is)
{
    SYMBOL *ptr;

    if (is->ptr_type
        || ((ptr = is->symbol)
            && (ptr->type & UNSIGNED)))
        return 1;
    return 0;
}

// lval.symbol - symbol table address,
// else 0 for constant lval.indirect -
// type indirect object to fetch, else
// 0 for static object lval.ptr_type -
// type pointer or array, else 0.
expression (int comma)
{
    LVALUE lval;
    int k;

    do {
        k = hier1 (&lval);
        if (k & FETCH)
            rvalue (&lval, k);
        if (!comma)
            return;
    } while (match (","));
}

// Assignment
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
        k = hier1 (lval2);
        if (k & FETCH)
            k = rvalue (lval2, k);
        store (lval);
        return 0;
    } else {
        fc = ch ();
        if (match ("-=") ||
            match ("+=") ||
            match ("*=") ||
            match ("/=") ||
            match ("%=") ||
            match (">>=") ||
            match ("<<=") ||
            match ("&=") || match ("^=") || match ("|=")) {
            if (!(k & FETCH)) {
                needlval ();
                return 0;
            }
            if (lval->indirect)
                gen_push (k);
            k = rvalue (lval, k);
            gen_push (k);
            k = hier1 (lval2);
            if (k & FETCH)
                k = rvalue (lval2);
            switch (fc) {
            case '-':{
                    if (dbltest (lval, lval2)) {
                        gen_mul_const (lval->ptr_type,
                                      lval->tagsym ? lval->
                                      tagsym->
                                      size : INTSIZE);
                    }
                    gen_sub ();
                    result (lval, lval2);
                    break;
                }
            case '+':{
                    if (dbltest (lval, lval2)) {
                        gen_mul_const (lval->ptr_type,
                                      lval->tagsym ? lval->
                                      tagsym->
                                      size : INTSIZE);
                    }
                    gen_add (lval, lval2);
                    result (lval, lval2);
                    break;
                }
            case '*':
                gen_mul ();
                break;
            case '/':
                if (nosign (lval) || nosign (lval2)) {
                    gen_udiv ();
                } else {
                    gen_div ();
                }
                break;
            case '%':
                if (nosign (lval) || nosign (lval2)) {
                    gen_umod ();
                } else {
                    gen_mod ();
                }
                break;
            case '>':
                if (nosign (lval)) {
                    gen_lsr ();
                } else {
                    gen_asr ();
                }
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
        } else
            return k;
    }
}

// ? : expression
hier1a (LVALUE * lval)
{
    int k, lab1, lab2;
    LVALUE lval2[1];

    k = hier1b (lval);
    blanks ();
    if (ch () != '?')
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER if (match ("?")) {
        gen_test_jump (lab1 = getlabel (), FALSE);
        k = hier1b (lval2);
        if (k & FETCH)
            k = rvalue (lval2, k);
        gen_jump (lab2 = getlabel ());
        def_local (lab1);
        blanks ();
        if (!match (":")) {
            error ("missing colon");
            return 0;
        }
        k = hier1b (lval2);
        if (k & FETCH)
            k = rvalue (lval2, k);
        def_local (lab2);
    } else
        return 0;
}

// "||"
hier1b (LVALUE * lval)
{
    int k, lab;
    LVALUE lval2[1];

    k = hier1c (lval);
    blanks ();
    if (!sstreq ("||"))
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER if (match ("||")) {
        gen_test_jump (lab = getlabel (), TRUE);
        k = hier1c (lval2);
        if (k & FETCH)
            k = rvalue (lval2, k);
        def_local (lab);
        gen_tobool ();
    } else
        return 0;
}

// "&&"
hier1c (LVALUE * lval)
{
    int k, lab;
    LVALUE lval2[1];

    k = hier2 (lval);
    blanks ();
    if (!sstreq ("&&"))
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER if (match ("&&")) {
        gen_test_jump (lab = getlabel (), FALSE);
        k = hier2 (lval2);
        if (k & FETCH)
            k = rvalue (lval2, k);
        def_local (lab);
        gen_tobool ();
    } else
        return 0;
}

// "|"
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
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (ch () == '|'
            && nch () != '|'
            && nch () != '=') {
            inbyte ();
            gen_push (k);
            k = hier3 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_or ();
            blanks ();
        } else
            return 0;
    }
}

// "^"
hier3 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier4 (lval);
    blanks ();
    if (ch () != '^'
        || nch () == '=')
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (ch () == '^'
            && nch () != '=') {
            inbyte ();
            gen_push (k);
            k = hier4 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_xor ();
            blanks ();
        } else
            return 0;
    }
}

// "&"
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
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (ch () == '&'
            && nch () != '&'
            && nch () != '=') {
            inbyte ();
            gen_push (k);
            k = hier5 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_and ();
            blanks ();
        } else
            return 0;
    }

}

// "==" and "!="
hier5 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier6 (lval);
    blanks ();
    if (!sstreq ("==") & !sstreq ("!="))
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (match ("==")) {
            gen_push (k);
            k = hier6 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_eq ();
        } else if (match ("!=")) {
            gen_push (k);
            k = hier6 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_neq ();
        } else
            return 0;
    }

}

// Comparison other than "==" and "!=".
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
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (match ("<=")) {
            gen_push (k);
            k = hier7 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_ulte ();
                continue;
            }
            gen_lte ();
        } else if (match (">=")) {
            gen_push (k);
            k = hier7 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_ugte ();
                continue;
            }
            gen_gte ();
        } else if (sstreq ("<")
                   && !sstreq ("<<")) {
            inbyte ();
            gen_push (k);
            k = hier7 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_ult ();
                continue;
            }
            gen_lt ();
        } else if (sstreq (">")
                   && !sstreq (">>")) {
            inbyte ();
            gen_push (k);
            k = hier7 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_ugt ();
                continue;
            }
            gen_gt ();
        } else
            return 0;
        blanks ();
    }

}

// "<<" and ">>"
hier7 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier8 (lval);
    blanks ();
    if (!sstreq (">>") &&
        !sstreq ("<<") || sstreq (">>=") || sstreq ("<<="))
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (sstreq (">>") && !sstreq (">>=")) {
            inbyte ();
            inbyte ();
            gen_push (k);
            k = hier8 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval)) {
                gen_lsr ();
            } else {
                gen_asr ();
            }
        } else if (sstreq ("<<") && !sstreq ("<<=")) {
            inbyte ();
            inbyte ();
            gen_push (k);
            k = hier8 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_asl ();
        } else
            return 0;
        blanks ();
    }

}

// "+" and "-"
hier8 (LVALUE * lval)
{
    int k;
    LVALUE lval2[1];

    k = hier9 (lval);
    blanks ();
    if (ch () != '+'
        && ch () != '-'
        || nch () == '=')
        return k;
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (match ("+")) {
            gen_push (k);
            k = hier9 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            // if left is pointer and right is int, scale right 
            if (dbltest (lval, lval2)) {
                gen_mul_const (lval->ptr_type,
                              lval->tagsym ? lval->tagsym->
                              size : INTSIZE);
            }
            // will scale left if right int pointer and left int 
            gen_add (lval, lval2);
            result (lval, lval2);
        } else if (match ("-")) {
            gen_push (k);
            k = hier9 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            /* if dbl, can only be: pointer - int, or
               pointer - pointer, thus,
               in first case, int is scaled up,
               in second, result is scaled down. */
            if (dbltest (lval, lval2)) {
                gen_mul_const (lval->ptr_type,
                              lval->tagsym ? lval->tagsym->
                              size : INTSIZE);
            }
            gen_sub ();
            // if both pointers, scale result 
            if (lval->ptr_type & CINT
                && lval2->ptr_type & CINT) {
                gen_div2 ();   // divide by intsize 
            }
            result (lval, lval2);
        } else
            return 0;
    }
}

// "*", "/" and "%"
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
    if (k & FETCH)
        k = rvalue (lval, k);
    FOREVER {
        if (match ("*")) {
            gen_push (k);
            k = hier10 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            gen_mul ();
        } else if (match ("/")) {
            gen_push (k);
            k = hier10 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_udiv ();
            } else {
                gen_div ();
            }
        } else if (match ("%")) {
            gen_push (k);
            k = hier10 (lval2);
            if (k & FETCH)
                k = rvalue (lval2, k);
            if (nosign (lval) || nosign (lval2)) {
                gen_umod ();
            } else {
                gen_mod ();
            }
        } else
            return 0;
    }

}

// "++", "--" and unary "-"
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
        k = rvalue (lval, k);
        gen_ptrinc (lval);
        store (lval);
        return HL_REG;
    } else if (match ("--")) {
        if (!((k = hier10 (lval)) & FETCH)) {
            needlval ();
            return 0;
        }
        if (lval->indirect)
            gen_push (k);
        k = rvalue (lval, k);
        gen_ptrdec (lval);
        store (lval);
        return HL_REG;
    } else if (match ("-")) {
        k = hier10 (lval);
        if (k & FETCH)
            k = rvalue (lval, k);
        gen_neg ();
        return HL_REG;
    } else if (match ("~")) {
        k = hier10 (lval);
        if (k & FETCH)
            k = rvalue (lval, k);
        gen_complement ();
        return HL_REG;
    } else if (match ("!")) {
        k = hier10 (lval);
        if (k & FETCH)
            k = rvalue (lval, k);
        gen_not ();
        return HL_REG;
    } else if (ch () == '*' && nch () != '=') {
        inbyte ();
        k = hier10 (lval);
        if (k & FETCH)
            k = rvalue (lval, k);
        if (ptr = lval->symbol)
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
                error ("illegal address");
            return 0;
        }
        ptr = lval->symbol;
        lval->ptr_type = ptr->type;
        if (lval->indirect) {
            if (k & DE_REG)
                gen_swap ();
            return HL_REG;
        }
        // Global and non-array.
        gen_load_1st ();
        output_string ((ptr = lval->symbol)->name);
        newline ();
        lval->indirect = ptr->type;
        return HL_REG;
    } else {
        k = hier11 (lval);
        if (match ("++")) {
            if (!(k & FETCH)) {
                needlval ();
                return 0;
            }
            if (lval->indirect)
                gen_push (k);
            k = rvalue (lval, k);
            gen_ptrinc (lval);
            store (lval);
            gen_ptrdec (lval);
            return HL_REG;
        } else if (match ("--")) {
            if (!(k & FETCH)) {
                needlval ();
                return 0;
            }
            if (lval->indirect)
                gen_push (k);
            k = rvalue (lval, k);
            gen_ptrdec (lval);
            store (lval);
            gen_ptrinc (lval);
            return HL_REG;
        } else
            return k;
    }

}

// Array subscript
hier11 (LVALUE * lval)
{
    int direct, k;
    SYMBOL *ptr;
    char sname[NAMESIZE];

    k = primary (lval);
    ptr = lval->symbol;
    blanks ();
    if (ch () == '['
        || ch () == '('
        || ch () == '.'
        || (ch () == '-'
            && nch () == '>'))
        FOREVER {
        if (match ("[")) {
            if (!ptr) {
                error ("can't subscript");
                junk ();
                needbrack ("]");
                return 0;
            } else if (ptr->identity == POINTER) {
                k = rvalue (lval, k);
            } else if (ptr->identity != ARRAY) {
                error ("can't subscript");
                k = 0;
            }
            gen_push (k);
            expression (YES);
            needbrack ("]");
            gen_mul_const (ptr->type,
                          tag_table[ptr->tagidx].size);
            gen_add (NULL, NULL);
            //lval->symbol = 0; 
            lval->indirect = ptr->type;
            lval->ptr_type = 0;
            k = FETCH | HL_REG;
        } else if (match ("(")) {
            if (!ptr) {
                callfunction (0);
            } else if (ptr->identity != FUNCTION) {
                k = rvalue (lval, k);
                callfunction (0);
            } else {
                callfunction (ptr);
            }
            lval->symbol = 0;
            k = 0;
        } else if ((direct = match (".")) || match ("->")) {
            if (!lval->tagsym) {
                error ("can't take member");
                junk ();
                return 0;
            }
            if (!symname (sname) ||
                (!(ptr = find_member (lval->tagsym, sname)))) {
                error ("unknown member");
                junk ();
                return 0;
            }
            if ((k & FETCH) && !direct)
                k = rvalue (lval, k);
            if (k == DE_REG)
                gen_swap ();

            // move pointer from struct begin to struct member 

            gen_add_const (ptr->offset);
            lval->symbol = ptr;
            // lval->indirect = lval->val_type = ptr->type 
            lval->indirect = ptr->type;
            lval->ptr_type = 0;
            lval->tagsym = NULL_TAG;
            if (ptr->type == STRUCT) {
                lval->tagsym = &tag_table[ptr->tagidx];
            }
            if (ptr->identity == POINTER) {
                lval->indirect = CINT;
                lval->ptr_type = ptr->type;
                //lval->val_type = CINT; 
            }
            if (ptr->identity == ARRAY ||
                (ptr->type == STRUCT
                 && ptr->identity == VARIABLE)) {
                // array or struct 
                lval->ptr_type = ptr->type;
                //lval->val_type = CINT; 
                k = 0;
            } else {
                k = FETCH | HL_REG;
            }
        } else
            return k;
        }
    if (!ptr)
        return k;
    if (ptr->identity == FUNCTION) {
        gen_load_1st ();
        output_string (ptr);
        newline ();
        return 0;
    }
    return k;
}
