#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"

find_tag (char *sname)
{
    int index;

    index = 0;
    while (index < tag) {
        if (astreq (sname, tags[index].name, NAMEMAX))
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
    int member;

    member = tag->member;
    while (member < tag->member + tag->num_members) {
        if (!strcmp (members[member].name, sname))
            return &members[member];
        member++;
    }
    return 0;
}

add_member (char *sname, char identity,
            char type, int offset,
            int storage_class,
            int member_size)
{
    char *buffer_ptr;

    // Allocate entry in member table.
    SYMBOL *symbol;
    if (member >= NUMMEMB) {
        error ("member table overflow");
        return 0;
    }
    symbol = &members[member];
    // Copy over name.
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    // Save type info.
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage_class;
    symbol->offset = offset;
    // TODO: Rename struct_size to compound_size.
    symbol->struct_size = member_size;

    // Step to free entry for next member.
    member++;
}

define_struct (char *sname, int storage,
               int is_struct)
{
    TAG_SYMBOL *symbol;
    char *s;

    // Allocate info.
    if (tag >= NUMTAG) {
        error ("struct table overflow");
        return 0;
    }
    symbol = &tags[tag];
    // Copy over name.
    s = symbol->name;
    while (alphanumeric (*s++ = *sname++));
    // TODO: Not sure if this means something.
    symbol->size = 0;
    // Set index of first member in
    // member table.
    symbol->member = member;

    // Process members.
    needbrack ("{");
    do {
        do_declarations (storage,
                         &tags[tag],
                         is_struct);
    } while (!match ("}"));

    // Step to next free tag for next struct.
    symbol->num_members = member - symbol->member;
    return tag++;
}
