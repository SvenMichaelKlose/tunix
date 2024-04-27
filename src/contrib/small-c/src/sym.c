#include <stdio.h>

#include "defs.h"
#include "data.h"
#include "ir.h"
#include "gen.h"
#include "io.h"
#include "lex.h"
#include "primary.h"
#include "initials.h"
#include "struct.h"
#include "sym.h"

// Declare a static variable.
// is_struct:
//       Tells if mtag is a struct or
//       union or no meaning.
// mtag: Tag of struct whose members are
//       being declared, or 0.
//       (parent struct).
// otag: Tag of struct object being
//       declared.  Only used with mtag.
void
declare_global (int type, int storage,
                TAG_SYMBOL * mtag,
                int otag,
                int is_struct)
{
    int     dim, identity;
    char    sname[NAMESIZE];
    SYMBOL  *symbol;
    FOREVER {
        FOREVER {
            if (endst ())
                return;
            dim = 1;
            identity = match ("*") ?
                POINTER :
                VARIABLE;
            if (!symname (sname))
                illname ();
            if (find_global (sname))
                multidef (sname);
            if (match ("[")) {
                dim = needsub ();
                identity = ARRAY;
            }
            // Add symbol.
            if (!mtag) {
                // Real variable, not a struct/union member.
                identity = initials (sname, type, identity, dim, otag);
                symbol = add_global (sname, identity, type,
                                     (!dim ? -1 : dim), storage);
                if (type == STRUCT)
                    symbol->tag = otag;
                break;
            }
            symbol = add_member (sname, identity, type,
                                 type & CINT ? dim * INTSIZE : dim);
            // store (correctly scaled) size of member.
            if (identity == POINTER)
                type = CINT;
            scale_const (type, otag, &dim);
            if (is_struct) {
                symbol->offset = mtag->size;
                symbol->struct_size = (type & CINT) ? dim * INTSIZE : dim;
                mtag->size += dim;
            } else {
                symbol->offset = 0;
                if (mtag->size < dim)
                    mtag->size = dim;
            }
        }
        if (!match (","))
            return;
    }
}

// Initialize global objects.
// Returns 1 if variable is initialized.
int
initials (char *symbol_name, int type,
          int identity, int dim,
          int otag)
{
    int dim_unknown = 0;
    litptr = 0;
    // Allow for xx[] = {..}; decl.
    if (!dim)
        dim_unknown = 1;
    if (!(type & CCHAR) && !(type & CINT)
        && !(type == STRUCT)) {
        perror ("unsupported storage size");
        return 0;
    }
    if (match ("=")) {
        // an array or struct 
        if (match ("{")) {
            // aggregate initialiser 
            if ((identity == POINTER
                 || identity == VARIABLE)
                && type == STRUCT) {
                // aggregate is structure or pointer to structure 
                dim = 0;
                struct_init (&tags[otag], symbol_name);
            } else {
                while ((dim > 0) || (dim_unknown)) {
                    if (identity == ARRAY && type == STRUCT) {
                        // array of struct 
                        needbrack ("{");
                        struct_init (&tags[otag], symbol_name);
                        --dim;
                        needbrack ("}");
                    } else if (init (symbol_name, type, identity, &dim, 0))
                        dim_unknown++;
                    if (!match (","))
                        break;
                }
                if (!--dim_unknown)
                    identity = POINTER;
            }
            needbrack ("}");
        } else
            // single constant 
            init (symbol_name, type, identity, &dim, 0);
    }
    return identity;
}

void
struct_init (TAG_SYMBOL * tag,
             char *symbol_name)
{
    int dummy_dim, member;
    int nmembers, first_member;
    member = first_member = tag->first_member;
    nmembers = tag->num_members;
    while (member < first_member + nmembers) {
        init (symbol_name,
              members[first_member + member].type,
              members[first_member + member].identity,
              &dummy_dim,
              tag);
        member++;
        if (!match (",")
            && (member != (first_member + nmembers))) {
            perror ("struct initialisation incomplete");
            return;
        }
    }
}

// Evaluate one initializer,
// add data to table.
int
init (char *symbol_name, int type,
      int identity, int *dim,
      TAG_SYMBOL * tag)
{
    int value, number_of_chars;
    if (identity == POINTER) {
        perror ("cannot assign to pointer");
        return 0;
    }
    if (quoted_string (&value)) {
        if (identity == VARIABLE
            || !(type & CCHAR)) {
            perror ("found string: must assign to char pointer or array");
            return 0;
        }
        number_of_chars = litptr - value;
        *dim = *dim - number_of_chars;
        while (number_of_chars > 0) {
            add_data_initials (symbol_name, CCHAR, litq[value++], tag);
            number_of_chars = number_of_chars - 1;
        }
    } else if (number (&value)) {
        add_data_initials (symbol_name, CINT, value, tag);
        *dim = *dim - 1;
    } else if (quoted_char (&value)) {
        add_data_initials (symbol_name, CCHAR, value, tag);
        *dim = *dim - 1;
    } else
        return 0;
    return 1;
}

// Declare local variables.
// Works just like declglb(), but
// modifies machine stack and adds
// symbol table entry with appropriate
// stack offset to find it again.
void
declare_local (int typ, int stclass,
               int otag)
{
    int  k, j;
    char sname[NAMESIZE];
    FOREVER {
        FOREVER {
            if (endst ())
                return;
            j = match ("*") ?
                POINTER :
                VARIABLE;
            if (!symname (sname))
                illname ();
            if (-1 != find_local (sname))
                multidef (sname);
            if (match ("[")) {
                k = needsub ();
                if (k) {
                    j = ARRAY;
                    if (typ & CINT)
                        k *= INTSIZE;
                    else if (typ == STRUCT)
                        k *= tags[otag].size;
                } else {
                    j = POINTER;
                    k = INTSIZE;
                }
            } else {
                if (j == POINTER)
                    k = INTSIZE;
                else {
                    switch (typ) {
                    case CCHAR:
                    case UCHAR:
                        k = 1;
                        break;
                    case STRUCT:
                        k = tags[otag].size;
                        break;
                    default:
                        k = INTSIZE;
                    }
                }
            }
            if (stclass != LSTATIC) {
                stkp = gen_modify_stack (stkp - k);
                current_symbol_table_idx = add_local (sname, j, typ, stkp, AUTO);
            } else
                current_symbol_table_idx = add_local (sname, j, typ, k, LSTATIC);
            // local structs need their tag set 
            if (typ == STRUCT)
                symbol_table[current_symbol_table_idx].tag = otag;
            break;
        }
        if (!match (","))
            return;
    }
}

// Get array size.
int
needsub ()
{
    int num[1];
    if (match ("]"))
        return 0;
    if (!number (num)) {
        perror ("must be constant");
        num[0] = 1;
    }
    if (num[0] < 0) {
        perror ("negative size illegal");
        num[0] = (-num[0]);
    }
    needbrack ("]");
    return num[0];
}

// Find global symbol.
SYMBOL *
find_global (char *sname)
{
    int idx;
    idx = 0;
    while (idx < global_table_index) {
        if (astreq (sname, symbol_table[idx].name, NAMEMAX))
            return &symbol_table[idx];
        idx++;
    }
    return 0;
}

// Find local symbol.
int
find_local (char *sname)
{
    int idx;
    idx = local_table_index;
    while (idx >= NUMBER_OF_GLOBALS) {
        idx--;
        if (astreq (sname, symbol_table[idx].name, NAMEMAX))
            return idx;
    }
    return -1;
}

// Add global symbol.
SYMBOL *
add_global (char *sname, int identity,
            int type, int offset,
            int storage)
{
    SYMBOL *symbol;
    char *buffer_ptr;
    if (symbol = find_global (sname))
        return symbol;
    if (global_table_index >= NUMBER_OF_GLOBALS) {
        perror ("global symbol table overflow");
        return NULL;
    }
    symbol = &symbol_table[global_table_index++];
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage;
    symbol->offset = offset;
    return symbol;
}

// Add new symbol to local table.
int
add_local (char *sname, int identity,
           int type, int offset,
           int storage_class)
{
    int     k;
    SYMBOL  *symbol;
    char    *buffer_ptr;
    if ((current_symbol_table_idx = find_local (sname)) > -1)
        return current_symbol_table_idx;
    if (local_table_index >= NUMBER_OF_GLOBALS + NUMBER_OF_LOCALS) {
        perror ("local symbol table overflow");
        return 0;
    }
    current_symbol_table_idx = local_table_index;
    symbol = &symbol_table[current_symbol_table_idx];
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage_class;
    if (storage_class == LSTATIC) {
        gen_data_segment ();
        def_local (k = getlabel ());
        gen_bss ();
        outn (offset);  // TODO: Number of BSS bytes?
        gen_code_segment ();
        offset = k;
    }
    symbol->offset = offset;
    local_table_index++;
    return current_symbol_table_idx;
}

// Test if next input string is legal
// symbol name.
int
symname (char *sname)
{
    int k;
    blanks ();
    if (!alpha (ch ()))
        return 0;
    k = 0;
    while (alphanumeric (ch ()))
        sname[k++] = gch ();
    sname[k] = 0;
    return 1;
}

void
illname ()
{
    perror ("illegal symbol name");
}

void
multidef (char *symbol_name)
{
    perror ("already defined");
    gen_comment ();
    outs (symbol_name);
    newline ();
}
