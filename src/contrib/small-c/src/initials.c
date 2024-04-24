#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"

// Add new symbol to table, initialise
// begin position in data array.
add_symbol_initials (char *symbol_name,
                     char type)
{
    strcpy (initials_table[initials_idx].name, symbol_name);
    initials_table[initials_idx].type = type;
}

// Find symbol in table, count position
// in data array.
find_symbol_initials (char *symbol_name)
{
    int result = 0;
    initials_data_idx = 0;
    for (initials_idx = 0;
         initials_table[initials_idx].type;
         initials_idx++) {
        if (initials_idx >= NUMBER_OF_GLOBALS)
            error ("initials table overrun");

        if (astreq (symbol_name, &initials_table[initials_idx].name, NAMEMAX)) {
            result = 1;
            break;
        } else {
            // Move to next symbol
            // count position in data
            // array.
            initials_data_idx += initials_table[initials_idx].data_len;
        }
    }
    return result;
}

// Add data to symbol in table.
add_data_initials (char *symbol_name, int type, int value,
                   TAG_SYMBOL * tag)
{
    int position;
    if (!find_symbol_initials (symbol_name))
        add_symbol_initials (symbol_name, tag ? STRUCT : type);
    if (tag) {
        // Find number of members, dim
        // is total number of values
        // added.
        int index =
            initials_table[initials_idx].dim %
            tag->num_members;
        int member_type =
            members[tag->first_member + index].type;
        // add it recursively.
        add_data_initials (symbol_name, member_type, value, 0);
    } else {
        position = initials_table[initials_idx].data_len;
        if (type & CCHAR) {
            initials_data_table[initials_data_idx + position] = 0xff & value;
            initials_table[initials_idx].data_len += 1;
        } else if (type & CINT) {

            initials_data_table[initials_data_idx + position] = (0xff00 & value) >> 8;

            initials_data_table[initials_data_idx + position + 1] = 0xff & value;
            initials_table[initials_idx].data_len += INTSIZE;
        }
        initials_table[initials_idx].dim += 1;
    }
}

// Get number of data items for given
// symbol.
get_size (char *symbol_name)
{
    int result = 0;
    if (find_symbol_initials (symbol_name))
        result = initials_table[initials_idx].dim;
    return result;
}

// Get item at position.
get_item_at (char *symbol_name,
             int position,
             TAG_SYMBOL * tag)
{
    int result = 0, i, type;
    if (find_symbol_initials (symbol_name)) {
        if (initials_table[initials_idx].type & CCHAR) {
            result = initials_data_table[initials_data_idx + position];
        } else if (initials_table[initials_idx].type & CINT) {
            position *= INTSIZE;
            result = (initials_data_table[initials_data_idx + position] << 8)
                         + (unsigned char) initials_data_table[initials_data_idx + position + 1];
        } else if (initials_table[initials_idx].type == STRUCT) {
            // Find number of members.
            int num_members = tag->num_members;
            // Point behind the last.
            // full struct.
            int index = (position / num_members) * tag->size;
            // Move to required member.
            for (i = 0; i < (position % num_members); i++) {
                type = members[tag->first_member + i].type;
                index += (type & CCHAR) ? 1 : INTSIZE;
            }
            // Get value.
            type = members[tag->first_member + i].type;
            result = (type & CCHAR) ?
                initials_data_table[initials_data_idx + index] :
                (initials_data_table [initials_data_idx + index] << 8)
                    + (unsigned char) initials_data_table[initials_data_idx + index + 1];
        }
    }
    return result;
}
