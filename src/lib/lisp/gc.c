// Mark-and-sweep garbage collection

// Trace objects and mark them.
void
mark (lispptr x)
{
    if (!MARKED(x)) {
        MARK(x);
        if (CONSP(x)) {
            // Loop over list.
            while (CONSP(x)) {
                mark (CAR(x));
                x = CDR(x);
                MARK(x);
            }
        } else if (SYMBOLP(x))
            mark (SYMBOL_VALUE(x));
    }
}

char * s;   // Source
char * d;   // Destination
uchar  c;   // Count
char * xlat;
char * minxlat;

char *
relocate_ptr (char * x)
{
    rel = 0;
    while (p = xlat; p != maxxlat;) {
        if (*+++p > x)
            return x - rel;
        rel += *++p;
    }
}

// Copy marked objects over deleted ones.
// Make list of addresses of deleted objects and their size
// for the pointer relocation pass.
void
sweep ()
{
    s = d = (char *) HEAP_START;
    xlat  = (char *) HEAP_END;
    // Calculate lowest address of xlat table.
    minxlat = heap + sizeof (char *) + sizeof (char);
    while ((type = *s)) {
        c = OBJSIZE(s);
        if (!TRACEDP(s)) {
            // Log deleted object;
            *--xlat = c; // Address
            *--xlat = s; // Size

            // Break if xlat table is full.
            if (xlat <= minxlat)
                return; // Will re-run after relocations.
        } else {
            // Copy object.
            *d++ = *s++ & MASK_MARKED;
            while (--c)
                *d++ = *s++;
        }
    }
    *d = 0;
    heap = d;
    sweep_done = TRUE;
}

void
relocate ()
{
}

void
gc ()
{
    mark (universe);
    sweep_done = FALSE;
    s = d = heap;  // Relocation pointers.
    while (!sweep_done) {
        sweep ();
        relocate ();
    }
}
