#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "data.h"
#include "lex.h"
#include "main.h"
#include "struct.h"

int
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
    for (member = tag->first_member;
         member < tag->first_member + tag->num_members;
         member++)
        if (!strcmp (members[member].name, sname))
            return &members[member];
    return 0;
}

SYMBOL *
add_member (char *sname, char identity,
            char type, int struct_size)
{
    char   *buffer_ptr;
    SYMBOL *symbol;
    if (++member >= NUMMEMB) { // XXX Weird preinc. (smk)
        perror ("member table overflow");
        return 0;
    }
    symbol = &members[member];
    buffer_ptr = symbol->name;
    while (alphanumeric (*buffer_ptr++ = *sname++));
    symbol->identity = identity;
    symbol->type = type;
    symbol->struct_size = struct_size;
    return symbol;
}

int
define_struct (char *sname, int storage,
               int is_struct)
{
    TAG_SYMBOL *symbol;
    char *s;

    // Allocate info.
    if (tag >= NUMTAG) {
        perror ("struct table overflow");
        return 0;
    }
    symbol = &tags[tag];

    // Populate info.
    s = symbol->name;
    while (alphanumeric (*s++ = *sname++));
    symbol->size = 0;
    symbol->first_member = member - 1;

    // Process members.
    needbrack ("{");
    do {
        do_declarations (storage, symbol, is_struct);
    } while (!match ("}"));
    symbol->num_members = member - symbol->first_member + 1;

    // Step to next free tag.
    return tag++;
}
