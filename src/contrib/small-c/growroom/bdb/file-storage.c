#include <stdio.h>

#include "bdb.h"

int
bdb_file_write (bdb *db, dbid_t ofs, void *data, size_t size)
{
    FILE * storage = (FILE *) db->storage;
    char * filename = "symbol.bdb";
    if (!storage) {
        if (!(storage = fopen (filename, "w+")))
            perror ("Cannot open file '%s' for writing.");
        db->storage = storage;
    }
    if (fseek (storage, ofs, SEEK_SET) < 0)
        perror ("symdb_write(): cannot seek.");
    return fwrite (data, 1, size, storage);
}

int
bdb_file_read (bdb *db, dbid_t ofs, void *data, size_t size)
{
    FILE * storage = (FILE *) db->storage;
    if (!storage)
        return 0;
    if (fseek (storage, ofs, SEEK_SET) < 0)
        perror ("symdb_read(): cannot seek.");
    return fread (data, 1, size, storage);
}
