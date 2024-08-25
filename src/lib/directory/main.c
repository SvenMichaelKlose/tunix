#include <string.h>
#include <stdlib.h>

#include <cbm.h>

#include "libdirectory.h"

struct cbm_dirent dirent;

// Free directory list.
void __fastcall__
free_directory_list (struct dirent * d)
{
    struct dirent * n;

    while (d) {
        n = d->next;
        free (d);
        d = n;
    }

    free (d);
}

struct dirent * __fastcall__
make_directory_list (unsigned *num_items)
{
    struct dirent * first_dirent = NULL;
    struct dirent * last_dirent = NULL;
    struct dirent * d;

    *num_items = 0;
    if (directory_open ())
        return NULL;

    while (1) {
        if (directory_read (&dirent))
            break;

        // Self or parent directory.
        if (dirent.type == CBM_T_HEADER || dirent.name[0] == '.' || (dirent.type == CBM_T_DIR && (!strcmp (".", dirent.name) || !strcmp ("..", dirent.name))))
            continue;

        // Update number of items.
        (*num_items)++;

        // Allocate dirent...
        d = malloc (sizeof (struct dirent));

        // ...and link it to the previous one.
        if (last_dirent)
            last_dirent->next = d;
        else
            first_dirent = d;
        last_dirent = d;

        // Copy size, type and name.
        d->size = dirent.size;
        d->type = dirent.type;
        memcpy (&d->name, &dirent.name, 17);
    }
    directory_close ();

    return first_dirent;
}
