// A schematic C implementation of
// essential TUNIX functions.

typedef unsigned char uchar;
typedef uchar procid_t;
typedef uchar waitid_t;
typedef uchar fn_t;
typedef uchar bankid_t;
typedef uchar bankref_t;
typedef uchar driverid_t;
typedef uchar sighdl_t;
typedef uchar sigid_t;

#define MAX_FN          256
#define MAX_BANK        128
#define MAX_DRIVER      16
#define MAX_WAITING     16
#define MAX_DEV         32
#define MAX_HANDLERS    256
#define MAX_PROCS       32

#define ERROR   -1
#define OK      0

struct fn {
    fn_t next;
    fn_t prev;
};
struct fn gfns[MAX_FN];

uchar glfn_refs[MAX_FN];

struct bank {
    fn_t      next;
    fn_t      prev;
    bankref_t ref;
};
struct bank banks[MAX_BANK];

bankref_t bank_ref[MAX_BANK];

struct driver {
    driverid_t  next;
    driverid_t  prev;
    procid_t    pid;
    void *      vectors[32];
};
struct driver drivers[MAX_DRIVER];

struct signal {
    char todo;
};

struct proc {
    uchar     flags;
    procid_t  next;
    procid_t  prev;
    struct wait {
        waitid_t  next;
        waitid_t  prev;
        procid_t  pid;
    } waiting[MAX_WAITING];
    waitid_t       first_waiting;
    struct bank    lbanks[MAX_BANK];
    bankref_t      lbank_ref[MAX_BANK];
    struct fn      lfns[MAX_FN];
    fn_t           lfn_glfn[MAX_FN];
    driverid_t     devices[MAX_DEV];
    
    sighdl_t       signal_handlers[MAX_HANDLERS];
    sigid_t        pending_signals[MAX_HANDLERS];
    struct signal  signals;
} procs[MAX_PROCS];

procid_t pid;

#define PROC_ZOMBIE     0x10
#define PROC_RUNNING    0x40
#define PROC_SLEEPING   0x80

#define IS_RUNNING(x) (procs[x].flags | PROC_RUNNING)
#define IS_SLEEPING(x) (procs[x].flags | PROC_SLEEPING)

#define ALLOC_PROC_RUNNING() 0

#define MV_SLEEPING_RUNNING(x)
#define MV_RUNNING_SLEEPING(x)

///////////////////
// Process lists //
///////////////////

procid_t running;
procid_t sleeping;
procid_t zombie;

void
machdep_switch (procid_t id)
{
    (void) id;
}

void
machdep_fork (procid_t id)
{
    (void) id;
}

void
update_procdata (procid_t id)
{
    (void) id;
}

void
machdep_fork_init_child (void)
{
}

void
schedule ()
{
    procid_t next = procs[pid].next;
    if (!next
        && (next = running)
        && !IS_RUNNING(next))
        next = 0;
    if (next != pid)
        machdep_switch (next);
}

// Wannabe version.  Original still has
// machdep code in it.
char
fork (void)
{
    char cpid = ALLOC_PROC_RUNNING();;
    if (!cpid)
        return ERROR;
    machdep_fork (cpid);
    if (pid != cpid) {
        update_procdata (cpid);
        procs[cpid].flags = PROC_RUNNING;
        return cpid;
    }
    machdep_fork_init_child ();
    return OK;
}

int
suspend (char x)
{
    if (!IS_RUNNING(procs[x]))
        return ERROR;
    MV_RUNNING_SLEEPING(x);
    return OK;
}

int
resume (char x)
{
    if (!IS_SLEEPING(procs[x]))
        return ERROR;
    MV_SLEEPING_RUNNING(x);
    return OK;
}

void
free_drivers (procid_t pid)
{
    (void) pid;
}

void
free_iopages (procid_t pid)
{
    (void) pid;
}

void
free_lbanks (procid_t pid)
{
    (void) pid;
}

void
panic (void)
{
}

void
free_proc_resources (procid_t pid)
{
    (void) pid;
}

#define DRM(x, y, z)
#define LPUSH(x, y, z)

void
zombify (procid_t id)
{
    if (!procs[id].flags)
        panic ();
    free_proc_resources (id);
    if (IS_RUNNING(id))
        DRM(procs, running, id);
    else
        DRM(procs, sleeping, id);
    LPUSH(procs, zombie, id);
    procs[id].flags = PROC_ZOMBIE;
}

void
resume_waiting (procid_t id)
{
    char w = procs[id].first_waiting;
    if (w)
        resume (w);
}

#define LPOP(x, y, z) 0

char
lfn_to_glfn (fn_t x)
{
    fn_t glfn;
    if (glfn = procs[pid].lfn_glfn[x])
        return glfn;
    LPUSH(lfns, first_lfn, x);;
    LPOP(glfn, glfns, free_glfn);;
    glfn_refs[glfn]++;
    procs[pid].lfn_glfn[x] = glfn;
    return glfn;
}

void
free_lfns (procid_t pid)
{
    fn_t glfn;
    fn_t y;
/*
    DOLIST(y, procs[pid].lfns, procs[pid].first_lfn) {
        glfn = procs[pid].lfn_glfn[y];
        if (!--glfn_refs[glfn])
            LPUSH(glfns, procs[pid].first_glfn, glfn);
    }
*/
}

#define DPUSH(x, y)

bankid_t
balloc (void)
{
    char b = 0; //LPOP(banks);
    if (b) {
        DPUSH(procs[pid].lbanks, procs[pid].first_lbank);
        bank_ref[b]++;
        return b;
    }
    return 0;
}

void
free_lbank (bankid_t b)
{
    //DRM(procs[pid].lbanks, procs[pid].first_bank);
}

int
bfree (bankid_t b)
{
    if (!procs[pid].lbank_ref[b])
        return ERROR;
    free_lbank (b);
    if (!bank_ref[b])
        panic ();
    if (!--bank_ref[b])
        LPUSH(banks, free_bank, b);
    return OK;
}

void
bprocfree (void)
{
    char i;
    //DOLIST(i, procs[pid].lbanks, procs[pid].first_lbank)
        bfree (i);
}

int
main (void)
{
    fork ();
    return 0;
}
