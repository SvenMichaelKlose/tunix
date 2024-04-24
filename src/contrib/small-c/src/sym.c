#include <stdio.h>
#include "defs.h"
#include "data.h"
#include "ir.h"

// Declare a static variable.
// is_struct:
//       Tells if mtag is a struct or
//       union or no meaning.
// mtag: Tag of struct whose members are
//       being declared, or 0.
//       (parent struct).
// otag: Tag of struct object being
//       declared.  Only used with mtag.
declare_global (int type, int storage,
                TAG_SYMBOL * mtag,
                int otag,
                int is_struct)
{
    int dim, identity;
    char sname[NAMESIZE];

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
            if (find_global (sname) > -1)
                multidef (sname);
            if (match ("[")) {
                dim = needsub ();
                identity = ARRAY;
            }
            // Add symbol.
            if (!mtag) {
                // Real variable, not a struct/union member.
                identity = initials (sname, type, identity, dim, otag);
                add_global (sname, identity, type, (!dim ? -1 : dim), storage);
                if (type == STRUCT)
                    symbol_table[current_symbol_table_idx].tag = otag;
                break;
            } if (is_struct) {
                // structure member, mtag->size is offset 
                add_member (sname, identity, type, mtag->size, storage, (type & CINT) ? dim * INTSIZE : dim);
                // store (correctly scaled) size of member in tag table entry 
                if (identity == POINTER)
                    type = CINT;
                scale_const (type, otag, &dim);
                mtag->size += dim;
            } else {
                // union member, offset is always zero 
                add_member (sname, identity, type, 0, storage, (type & CINT) ? dim * INTSIZE : dim);
                // store maximum member size in tag table entry 
                if (identity == POINTER)
                    type = CINT;
                scale_const (type, otag, &dim);
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
        && !(type == STRUCT))
        error ("unsupported storage size");
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

// Initialise structure.
struct_init (TAG_SYMBOL * tag,
             char *symbol_name)
{
    int dim, member;

    member = tag->member;
    while (member < tag->member + tag->num_members) {
        init (symbol_name,
              members[tag->member + member].type,
              members[tag->member + member].identity, &dim, tag);
        ++member;
        if (!match (",")
            && (member != (tag->member + tag->num_members))) {
            error ("struct initialisation incomplete");
            break;
        }
    }
}

// Evaluate one initializer,
// add data to table.
init (char *symbol_name, int type,
      int identity, int *dim,
      TAG_SYMBOL * tag)
{
    int value, number_of_chars;
    if (identity == POINTER)
        error ("cannot assign to pointer"); // TODO
    if (quoted_string (&value)) {
        if (identity == VARIABLE
            || !(type & CCHAR))
            error ("found string: must assign to char pointer or array");
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
declare_local (int typ, int stclass,
               int otag)
{
    int k, j;
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
needsub ()
{
    int num[1];

    if (match ("]"))
        return 0;
    if (!number (num)) {
        error ("must be constant");
        num[0] = 1;
    }
    if (num[0] < 0) {
        error ("negative size illegal");
        num[0] = (-num[0]);
    }
    needbrack ("]");
    return num[0];
}

// Find global symbol.
find_global (char *sname)
{
    int idx;

    idx = 0;
    while (idx < global_table_index) {
        if (astreq (sname, symbol_table[idx].name, NAMEMAX))
            return idx;
        idx++;
    }
    return -1;
}

// Find local symbol.
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
add_global (char *sname, int identity,
            int type, int offset,
            int storage)
{
    SYMBOL *symbol;
    char *buffer_ptr;
    if ((current_symbol_table_idx = find_global (sname)) > -1)
        return current_symbol_table_idx;
    if (global_table_index >= NUMBER_OF_GLOBALS) {
        error ("global symbol table overflow");
        return 0;
    }
    current_symbol_table_idx = global_table_index;
    symbol = &symbol_table[current_symbol_table_idx];
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage;
    symbol->offset = offset;
    global_table_index++;
    return current_symbol_table_idx;
}

// Add new symbol to local table.
add_local (char *sname, int identity,
           int type, int offset,
           int storage_class)
{
    int k;
    SYMBOL *symbol;
    char *buffer_ptr;

    if ((current_symbol_table_idx = find_local (sname)) > -1)
        return current_symbol_table_idx;
    if (local_table_index >= NUMBER_OF_GLOBALS + NUMBER_OF_LOCALS) {
        error ("local symbol table overflow");
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

illname ()
{
    error ("illegal symbol name");
}

multidef (char *symbol_name)
{
    error ("already defined");
    gen_comment ();
    outs (symbol_name);
    newline ();
}
