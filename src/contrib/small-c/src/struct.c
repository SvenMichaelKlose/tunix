#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"

int
find_tag (char *sname)
{
    int index;

    index = 0;
    while (index < tag_table_index) {
        if (astreq (sname, tag_table[index].name, NAMEMAX))
            return index;
        ++index;
    }
    return -1;
}

// Determine if 'sname' is a member of
// the struct with tag 'tag'.
SYMBOL *
find_member (TAG_SYMBOL * tag, char *sname)
{
    int member_idx;

    member_idx = tag->member_idx;
    while (member_idx < tag->member_idx + tag->number_of_members) {
        if (strcmp (member_table[member_idx].name, sname) == 0)
            return &member_table[member_idx];
        ++member_idx;
    }
    return 0;
}

add_member (char *sname, char identity,
            char type, int offset,
            int storage_class,
            int member_size)
{
    char *buffer_ptr;
    SYMBOL *symbol;
    if (member_table_index >= NUMMEMB) {
        error ("symbol table overflow");
        return 0;
    }
    symbol = &member_table[member_table_index];
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage_class;
    symbol->offset = offset;
    // set size for arrays 
    symbol->struct_size = member_size;
    member_table_index++;
}

int
define_struct (char *sname, int storage,
               int is_struct)
{
    TAG_SYMBOL *symbol;
    char *s;

    if (tag_table_index >= NUMTAG) {
        error ("struct table overflow");
        return 0;
    }
    symbol = &tag_table[tag_table_index];
    s = symbol->name;
    while (alphanumeric (*s++ = *sname++));
    symbol->size = 0;
    symbol->member_idx = member_table_index;

    needbrack ("{");
    do {
        do_declarations (storage,
                         &tag_table[tag_table_index],
                         is_struct);
    } while (!match ("}"));
    symbol->number_of_members = member_table_index - symbol->member_idx;
    return tag_table_index++;
}
