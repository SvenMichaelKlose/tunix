// Portable RAM disk driver for TUNIX.
//
// Files data is queued up by the page.
// Each file holds a list of pages which
// grows every four pages.

#define RAMFS_CHUNK_PAGES   4

typedef struct _file {
    char *      name;
    uchar       namelen;
    uchar *     pages;
    uchar       npages;
    unsigned    size;
} file;

bool do_create;
bool do_read;
bool do_append;
bool do_write;

void
file_delete ()
{
    char n, p;

    // Take off directory.
    DRM(files, free_file);

    // Free pages.
    n = file->pages;
    for (i = 0; i < n; i++)
        if (p = file->pages[i])
            LPUSH(pages, free_page, p);
}

void
file_writeb (char c)
{
    if (!CURRENT_PAGE()) {
        p = palloc ();
        SET_FILE_PAGE(current_page, p);
    }
}

char
dopen ()
{
    if (ALREADY_OPEN())
        return derror (-1);
    if (process_filename ())
        return derror (-1);
    find_file ();
    if (do_create) {
        if (file)
            return derror (-1);
        create_file ();
        file->mode = M_WRITE;
        return 0;
    }
    if (!file)
        return derror (-1);
    f->ptr = f->base;
    if (do_append) {
        file->mode = M_WRITE;
        f->ptr = f->base + f->end;
        return;
    }
    file->mode = M_READ;
}

char
dbsout ()
{
}

void
prealloc_pages ()
{
}

int
main (int argc, char * argv[])
{
    prealloc_pages ();
    tunix_register (dev, vectors);
    terminate (0);
}
