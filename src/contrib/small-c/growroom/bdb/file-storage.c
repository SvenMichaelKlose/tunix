#include <stdlib.h>
#include <stdio.h>
#include <err.h>

#include "bdb.h"

int
bdb_file_write (bdb *db, dbid_t ofs, void *data, size_t size)
{
    FILE * storage = (FILE *) db->storage;

    // Open new file.
    if (!(db->flags & HAS_OPEN_STORAGE)) {
        if (!(storage = fopen ((char *) db->storage, "w+")))
            err (EXIT_FAILURE, "Cannot open file '%s' for writing.",
                 (char *) db->storage);

        // Replace storage info by file handle.
        db->storage = storage;
        db->flags |= HAS_OPEN_STORAGE;
    }
    if (fseek (storage, ofs, SEEK_SET) < 0)
        err (EXIT_FAILURE, "symdb_write(): cannot seek.");
    return fwrite (data, 1, size, storage);
}

int
bdb_file_read (bdb *db, dbid_t ofs, void *data, size_t size)
{
    FILE * storage = (FILE *) db->storage;
    if (!storage)
        return 0;
    if (fseek (storage, ofs, SEEK_SET) < 0)
        err (EXIT_FAILURE, "symdb_read(): cannot seek.");
    return fread (data, 1, size, storage);
}
