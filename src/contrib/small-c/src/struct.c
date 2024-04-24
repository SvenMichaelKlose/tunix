#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"

find_tag (char *sname)
{
    int i;
    for (i = 0; i < tag; i++)
        if (astreq (sname, tags[i].name, NAMEMAX))
            return i;
    return -1;
}

// Determine if 'sname' is a member of
// the struct with tag 'tag'.
SYMBOL *
find_member (TAG_SYMBOL * tag, char *sname)
{
    int member;
    for (member = tag->member;
         member < tag->member + tag->num_members;
         member++)
        if (!strcmp (members[member].name, sname))
            return &members[member];
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

    // Populate info.
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->storage = storage_class;
    symbol->offset = offset;
    symbol->struct_size = member_size;

    // Step to next free member.
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

    // Populate info.
    s = symbol->name;
    while (alphanumeric (*s++ = *sname++));
    symbol->size = 0;
    symbol->member = member;

    // Process members.
    needbrack ("{");
    do {
        do_declarations (storage, symbol, is_struct);
    } while (!match ("}"));
    symbol->num_members = member - symbol->member;

    // Step to next free tag.
    return tag++;
}
