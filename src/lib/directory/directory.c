#include <string.h>
#include <stdlib.h>

#include <cbm.h>

#include "libdirectory.h"

char
directory_open ()
{
    return cbm_opendir (2, 8, "$");
}

char __fastcall__
directory_read (struct cbm_dirent * dirent)
{
    return cbm_readdir (2, dirent);
}

void
directory_close ()
{
    cbm_closedir (2);
}
