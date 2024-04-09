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

/* For future directory support.
char __fastcall__
gcbm_enterdir (char * name)
{
    sprintf (message_buffer, "CD/%S/", name);
    cbm_open (15, 8, 15, message_buffer);
    cbm_read (15, message_buffer, 63);
    cbm_close (15);

    return 0;
}

void
gcbm_leavedir ()
{
}
*/

char __fastcall__
gcbm_open (char * name, char mode)
{
    return cbm_open (2, 8, mode, name);
}

int __fastcall__
gcbm_read (void * data, unsigned size)
{
    if (cbm_k_readst ())    /* cbm_read() returns 1 on end-of-file. */
        return 0;
    return cbm_read (2, data, size);
}

void
gcbm_close ()
{
    cbm_close (2);
}

void __fastcall__
fsb_free (struct dirent * d)
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
make_directory_list ()
{
    struct dirent * first_dirent = NULL;
    struct dirent * last_dirent = NULL;
    struct dirent * d;
    unsigned len = 0;

    gcbm_opendir ();
    while (1) {
        if (gcbm_readdir (&dirent))
            break;
        if (dirent.type == CBM_T_HEADER || dirent.name[0] == '.' || (dirent.type == CBM_T_DIR && (!strcmp (".", dirent.name) || !strcmp ("..", dirent.name))))
            continue;
        len++;
        d = malloc (sizeof (struct dirent));
        if (last_dirent)
            last_dirent->next = d;
        else
            first_dirent = d;
        last_dirent = d;

        d->size = dirent.size;
        d->type = dirent.type;
        memcpy (&d->name, &dirent.name, 17);
    }
    gcbm_closedir ();

    return first_dirent;
}
