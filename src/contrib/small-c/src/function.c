#include <stdio.h>
#include "defs.h"
#include "data.h"

int argtop;

// Begin a function.
// Called from parse(), this routine
// tries to make a function out of what
// follows.
// Modified version by  P.L. Woods.
newfunc ()
{
    char n[NAMESIZE];
    int idx, type;
    fexitlab = getlabel ();

    if (!symname (n)) {
        error ("illegal function or declaration");
        kill ();
        return;
    }
    if ((idx = find_global (n)) > -1) {
        if (symbol_table[idx].identity != FUNCTION)
            multidef (n);
        else if (symbol_table[idx].offset == FUNCTION)
            multidef (n);
        else
            symbol_table[idx].offset = FUNCTION;
    } else
        add_global (n, FUNCTION, CINT, FUNCTION, PUBLIC);
    if (!match ("("))
        error ("missing open paren");
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
                error ("illegal argument name");
                junk ();
            }
            blanks ();
            if (!streq (line + lptr, ")")) {
                if (!match (","))
                    error ("expected comma");
            }
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
                error ("wrong number args");
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
getarg (int t)
{
    int j, legalname, address,
        argptr, otag;
    char n[NAMESIZE];

    FOREVER {
        if (!argstk)
            return;
        // If a struct is being passed,
        // its tag must be read in
        // before checking if it is a
        // pointer.
        if (t == STRUCT) {
            if (!symname (n)) {
                // Make sure tag doesn't
                // contain odd symbols,
                // etc.
                illname ();
            }
            // struct tag undefined 
            if ((otag = find_tag (n)) == -1)
                error ("struct tag undefined");
        }
        if (match ("*"))
            j = POINTER;
        else
            j = VARIABLE;
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
                // set tagidx for struct arguments 
                if (t == STRUCT) {
                    if (j != POINTER) {
                        // because each argument takes exactly two bytes on the
                        // stack, whole structs can't be passed to functions */
                        error
                            ("only struct pointers, not structs, can be passed to functions");
                    }
                    symbol_table[argptr].tagidx = otag;
                }
            } else
                error ("expecting argument name");
        }
        argstk = argstk - INTSIZE;
        if (endst ())
            return;
        if (!match (","))
            error ("expected comma");
    }
}

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
        if (type) {
            doLocalAnsiArgument (type);
        } else {
            error ("wrong number args");
            break;
        }
        if (match (",")) {
            type = get_type ();
            continue;
        }
        if (match (")"))
            break;
    }
}

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
            error ("struct tag undefined");
    }
    if (match ("*"))
        identity = POINTER;
    else
        identity = VARIABLE;
    if (symname (symbol_name)) {
        if (find_local (symbol_name) > -1) {
            multidef (symbol_name);
        } else {
            argptr = add_local (symbol_name, identity, type, 0, AUTO);
            argstk = argstk + INTSIZE;
            ptr = local_table_index;
            // if argument is a struct, properly set the argument's tagidx.
            if (type == STRUCT) {
                if (identity != POINTER) {
                    // Because each argument takes exactly two bytes
                    // on the stack, whole structs can't be passed to functions.
                    error
                        ("only struct pointers, not structs, can be passed to functions");
                }
                symbol_table[argptr].tagidx = otag;
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
        error ("illegal argument name");
        junk ();
    }
    if (match ("[")) {
        while (inbyte () != ']') {
            if (endst ()) {
                break;
            }
        }
        identity = POINTER;
        symbol_table[argptr].identity = identity;
    }
}
