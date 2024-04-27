#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "sym.h"
#include "io.h"
#include "ir.h"
#include "struct.h"
#include "stmt.h"
#include "lex.h"
#include "gen.h"
#include "function.h"

int argtop;

// Begin a function.
// Called from parse(), this routine
// tries to make a function out of what
// follows.
// Modified version by P.L. Woods.
void
newfunc ()
{
    char     n[NAMESIZE];
    int      type;
    SYMBOL * symbol;
    fexitlab = getlabel ();

    if (!symname (n)) {
        perror ("illegal function or declaration");
        kill ();
        return;
    }
    // Reuse symbol entry, if it's a function.
    if (symbol = find_global (n)) {
        if (symbol->identity != FUNCTION
            || symbol->offset == FUNCTION)
            multidef (n);
        else
            symbol->offset = FUNCTION;
    } else
        add_global (n, FUNCTION, CINT, FUNCTION, PUBLIC);

    if (!match ("("))
        perror ("missing open paren");
    def_global (n);
    local_table_index = NUMBER_OF_GLOBALS;      //locptr = STARTLOC; 
    argstk = 0;
    // ANSI style argument declaration 
    if (!doAnsiArguments ()) {
        // K&R style argument declaration 
        while (!match (")")) {
            if (symname (n)) {
                if (find_local (n) > -1)
                    multidef (n);
                else {
                    add_local (n, 0, 0, argstk, AUTO);
                    argstk = argstk + INTSIZE;
                }
            } else {
                perror ("illegal argument name");
                junk ();
            }
            blanks ();
            if (!streq (line + lptr, ")")
                && !match (","))
                perror ("expected comma");
            if (endst ())
                break;
        }
        stkp = 0;
        argtop = argstk;
        while (argstk) {
            if (type = get_type ()) {
                getarg (type);
                need_semicolon ();
            } else {
                perror ("wrong number args");
                break;
            }
        }
    }
    statement (YES);
    def_local (fexitlab);
    gen_modify_stack (0);
    gen_ret ();
    stkp = 0;
    local_table_index = NUMBER_OF_GLOBALS;      //locptr = STARTLOC; 
}

// Declare argument types.
// Called from newfunc(), this routine
// adds an entry in the local symbol
// table for each named argument
// (Completely rewritten version by P.L.
// Woods.)
// @param t argument type (char, int)
void
getarg (int t)
{
    int j, legalname, address,
        argptr, otag;
    char n[NAMESIZE];

    FOREVER {
        if (!argstk)
            return;
        if (t == STRUCT) {
            if (!symname (n))
                illname ();
            if ((otag = find_tag (n)) == -1)
                perror ("struct tag undefined");
        }
        j = match ("*") ?
                POINTER :
                VARIABLE;
        if (!(legalname = symname (n)))
            illname ();
        if (match ("[")) {
            while (inbyte () != ']')
                if (endst ())
                    break;
            j = POINTER;
        }
        if (legalname) {
            if ((argptr = find_local (n)) > -1) {
                symbol_table[argptr].identity = j;
                symbol_table[argptr].type = t;
                address =
                    argtop - symbol_table[argptr].offset;
                symbol_table[argptr].offset = address;
                // set tag for struct arguments 
                if (t == STRUCT) {
                    if (j != POINTER)
                        perror ("only struct pointers, not structs, can be passed to functions");
                    symbol_table[argptr].tag = otag;
                }
            } else
                perror ("expected argument name");
        }
        argstk = argstk - INTSIZE;
        if (endst ())
            return;
        if (!match (","))
            perror ("expected comma");
    }
}

int
doAnsiArguments ()
{
    int type;
    type = get_type ();
    if (!type)
        // No type detected, revert back
        // to K&R style 
        return 0;
    argtop = argstk;
    argstk = 0;
    FOREVER {
        if (type)
            doLocalAnsiArgument (type);
        else {
            perror ("wrong number args");
            break;
        }
        if (match (",")) {
            type = get_type ();
            continue;
        }
        if (match (")"))
            break;
    }
    return 1;
}

void
doLocalAnsiArgument (int type)
{
    char symbol_name[NAMESIZE];
    int identity, address, argptr, ptr, otag;
    // If a struct is being passed, its
    // tag must be read in before
    // checking if it is a pointer.
    if (type == STRUCT) {
        if (!symname (symbol_name))
            illname ();
        if ((otag = find_tag (symbol_name)) == -1)
            perror ("struct tag undefined");
    }
    identity = match ("*") ?
        POINTER :
        VARIABLE;
    if (symname (symbol_name)) {
        if (find_local (symbol_name) > -1)
            multidef (symbol_name);
        else {
            argptr = add_local (symbol_name, identity, type, 0, AUTO);
            argstk = argstk + INTSIZE;
            ptr = local_table_index;
            // if argument is a struct, properly set the argument's tag.
            if (type == STRUCT) {
                if (identity != POINTER)
                    perror ("only struct pointers, not structs, can be passed to functions");
                symbol_table[argptr].tag = otag;
            }
            // modify stack offset as we push more params 
            while (ptr != NUMBER_OF_GLOBALS) {
                ptr = ptr - 1;
                address = symbol_table[ptr].offset;
                symbol_table[ptr].offset =
                    address + INTSIZE;
            }
        }
    } else {
        perror ("illegal argument name");
        junk ();
    }
    if (match ("[")) {
        while (inbyte () != ']')
            if (endst ())
                break;
        identity = POINTER;
        symbol_table[argptr].identity = identity;
    }
}
