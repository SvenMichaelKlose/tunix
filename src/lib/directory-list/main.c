#include <string.h>
#include <stdlib.h>

#include <cbm.h>

#include "directory-list.h"

struct cbm_dirent dirent;

char
gcbm_opendir ()
{
    return cbm_opendir (2, 8, "$");
}

char __fastcall__
gcbm_readdir (struct cbm_dirent * dirent)
{
    return cbm_readdir (2, dirent);
}

void
gcbm_closedir ()
{
    cbm_closedir (2);
}

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
    gcbm_opendir ();
    while (1) {
        if (gcbm_readdir (&dirent))
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
    gcbm_closedir ();

    return first_dirent;
}
