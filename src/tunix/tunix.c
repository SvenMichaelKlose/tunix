// A schematic C implementation of
// essential TUNIX functions.

typedef unsigned char uchar;
typedef uchar procid_t;
typedef uchar waitid_t;
typedef uchar fn_t;

struct fn {
    fn_t next;
    fn_t prev;
};
struct fn gfns[MAX_FN];

struct bank {
    fn_t      next;
    fn_t      prev;
    bankref_t ref;
};
struct bank banks[MAX_BANK];

struct driver {
    driverid_t  next;
    driverid_t  prev;
    procid_t    pid;
    void *      vectors[32];
};
struct driver drivers[MAX_DRIVER];

struct proc {
    uchar     flags;
    procid_t  next;
    procid_t  prev;
    struct wait {
        waitid_t  next;
        waitid_t  prev;
        procid_t  pid;
    } waiting[MAX_WAITING];
    waitid_t     first_waiting;
    struct bank  lbanks[MAX_BANK];
    struct fn    lfns[MAX_FN];
    fn_t         lfn_glfn[MAX_FN];
    driverid_t   devices[MAX_DEV];
    
} procs[MAX_PROCS];
procid_t pid;
procid_t running;
procid_t sleeping;
procid_t zombie;

void
schedule ()
{
    procid_t next = proc->next;
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
    char cpid = ALLOC_PROC_RUNNING();
    if (!cpid)
        return -1;
    machdep_fork (cpid);
    if (pid != cpid) {
        update_procdata (cid);
        procs[cpid].flags = PROC_RUNNING;
        return cpid;
    }
    machdep_fork_init_child ();
    return 0;
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
free_proc_resources (procid_t id)
{
    free_lfns (id);
    bproc_free (id);
    free_iopages (id);
    free_drivers (id);
}

void
zombify (procid_t id)
{
    if (!proc[id]->flags)
        panic ();
    free_proc_resources (id);
    if (PROC_IS_RUNNING(id))
        DRM(procs, running, id);
    else
        DRM(procs, sleeping, id);
    LPUSH(procs, zombie, id);
    proc[id].flags = PROC_ZOMBIE;
}

void
resume_waiting (procid_t id)
{
    char w = procs[id].first_wait;
    if (w)
        resume (w);
}

char
lfn_to_glfn (fn_t x)
{
    fn_t glfn;
    if (glfn = lfn_glfn[x])
        return glfn;
    LPUSH(lfns, first_lfn, x);
    LPOP(glfn, glfns, free_glfn);
    glfn_refs[glfn]++;
    return lfn_glfn[x] = glfn;
}

void
free_lfns (void)
{
    fn_t glfn;
    fn_t y;
    DOLIST(y, lfns, first_lfn) {
        glfn = lfn_glfn[y];
        if (!--glfn_refs[glfn])
            LPUSH(glfns, first_glfn, glfn);
    }
}

bank_t
balloc (void)
{
    char b = LPOP(banks);
    if (b) {
        DPUSH(lbanks, first_lbank);
        bank_refs[b]++;
        return b;
    }
    return 0;
}

void
free_lbank (bank_t b)
{
    DRM(lbanks, first_bank);
}

int
bfree (bank_t b)
{
    if (!lbanks_ref[b])
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
    DOLIST(i, lbanks, first_lbank)
        bfree (i);
}
