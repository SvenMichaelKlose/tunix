/*
 * KERNAL function emulation
 *
 * This is where the complex things happen, that's why
 * it is written in C.
 */

typedef struct _logical_file {
    upos    dir;    // The directory the file is in.
    upos    pos;    // Position of bfile.
    upos    ofs;    // Current position in file.
    char    mode;   // Read (0) or write (1).
} logical_file;

logical_file logical_files[256];

void
kernal_init ()
{
    char i;

    do {
        logical_files[i] = NULL;
    } while (++i);
}

void
kernal_open ()
{
    char * name = malloc (_FNLEN());
    upos found_file;

    if (!name) {
        set_error (ERR_OUT_OF_MEMORY);
        return;
    }

    copy_from_process (_FNAME(), name, _FNLEN());
    found_file = bfile_lookup_name (current_directory, name, _FNLEN());
    if (!found_file) {
        set_error (ERR_NOT_FOUND);
        return;
    }

    lf = alloc_logical_file (_LFN());
    if (!lf) {
        set_error (ERR_WRONG_LFN);
        return;
    }

    lf->dir = current_directory;
    lf->pos = found_file;
    lf->ofs = 0;

    set_ok ();
}
