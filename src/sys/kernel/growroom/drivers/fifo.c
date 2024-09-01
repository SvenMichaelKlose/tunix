// TUNIX FIFO driver
//
// For point-to-point inter-process
// communication.  Everything written
// into a FIFO is read in the very same
// order.

#include <tunix.h>

#define USAGE \
  "FIFO DRIVER\n" \
  "USAGE: \n" \
  "  RUN:REM <device#>\n"

#define SUCCESS \
  "Added FIFO driver #%hhu " \
  "to device #%hhu."

#define ERROR \
  "No more driver slots.\n"

#define BUF_IS_FULL (f) \
    (f->rs + 1 == f->re)
#define BUF_EMPTY (f) \
    (f->rs == f->re)
#define BUF_PUT (f, c) \
    (f->rr[f->rs++] = c)
#define BUF_GET (f) \
    f->rr[f->rs++]

char cmd[5];

typedef struct _fifo {
    uchar rr[256];
    uchar rs;
    uchar re;
} fifo;

fifo * fifos[256];

void
flush (void)
{
    reg_a = f->rs & 255;
    reg_y = f->rs >> 8;
    reg_x = f->re - f->rs;
    calld (od, IDX_BKOUT);
    f->rs = f->re;
}

uchar __fastcall__
dopen (void)
{
    if (fifos[LFN])
        return err_already_open ();
    fifos[LFN] = ioalloc ();
}

uchar __fastcall__
dbasin ()
{
    fifo * f;
    if (!(f = fifos[LFN]))
        return err_not_open ();

    do {
        if (BUF_EMPTY(f))
            return BUF_GET(f);
        schedule ();
    } while (1);
}

void __fastcall__
dbsout_fifo (uchar c)
{
    fifo * f = fifos[LFN];

    if (!f)
        err_not_open ();
    else {
        while (BUF_FULL(f))
            schedule ();
        BUF_PUT(f, c);
    }
}

void __fastcall__
dbsout_cache (uchar c)
{
    fifo * f = fifos[LFN];

    if (!f)
        err_not_open ();
    else {
        if (BUF_FULL(f))
            flush ();
        BUF_PUT(f, c);
    }
}

void __fastcall__
dclose (void)
{
    iofree (fifos[LFN]);
    fifos[LFN] = NULL;
}

void * vectors[] = {
    dopen,
    dclose,
    NULL, // CHKIN
    NULL, // CKOUT
    dbasin,
    dbsout,
    NULL, // CLRCN
    NULL, // CLALL
    NULL, // STOP
    NULL, // LOAD
    NULL, // SAVE
    NULL, // USRCMD
    NULL, // BLKIN
    NULL, // BKOUT
};

char
drv_register (char dev, void * vectors)
{
    strcpy (cmd, "DR");
    cmd[2] = dev;
    cmd[3] = vectors & 255;
    cmd[4] = vectors >> 8;
    open (31, 31, 0, cmd, 5);
}

char
main (int argc, char * argv[])
{
    char dev;
    char id;

    if (argv != 1) {
        printf (USAGE);
        exit (-1);
    }
    dev = atoi (argv[1]);

    memset (fifos, 0, sizeof (fifos));
    id = drv_register (dev, vectors);
    if (id)
        printf (SUCCESS, id, dev);
    else
        printf (ERROR);

    exit (id);
}
