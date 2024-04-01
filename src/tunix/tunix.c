// A schematic C implementation of
// essential TUNIX functions.

// Switch to other reason if it's time.
void
schedule ()
{
    // Machine-dependent.
}

char
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
free_lbank (char b)
{
    DRM(lbanks, first_bank);
}

void
bfree (char b)
{
    if (!bank_ref[b]) {
        __CARRY__ = 1;
    } else {
        LPUSH(banks, free_bank, b);
        DRM(lbanks, first_lbank, b);
    }
}

void
bprocfree (void)
{
    char i;
    DOLIST(i, lbanks, first_lbank)
        bfree (i);
}

char
lfn_to_glfn (char x)
{
    char glfn, a;
    if (a = lfn_glfn[x])
        return a;
    LPUSH(lfns, first_lfn, x);
    LPOP(glfn, glfns, glfns);
    glfn_refs[glfn]++;
    return lfn_glfn[x] = glfn;
}

void
free_lfns (void)
{
    char glfn;
    char y;
    DOLIST(y, lfns, first_lfn) {
        glfn = lfn_glfn[y];
        if (!--glfn_refs[glfn])
            LPUSH(glfns, glfns, glfn);
    }
}

// Wannabe version.  Original still has
// machdep code in it.
char
fork (void)
{
    char npid = ALLOC_PROC_RUNNING();
    if (!npid) {
        __CARRY__ = 1;
        return -1;
    }
    machdep_fork (npid);
    if (pid != npid) {
        procs[npid].flags = PROC_RUNNING;
        increment_lfn_refs (nid);
    } else {
        // Map in child banks.
    }
}

void
suspend (char x)
{
    if (procs[x].flags <= 0)
        ERROR();
    MV_RUNNING_SLEEPING(x);
}

void
resume (char x)
{
    if (procs[x].flags >= 0)
        ERROR();
    MV_SLEEPING_RUNNING(x);
}

void
zombify (char id)
{
    if (!proc[id])
        ERROR();
    free_lfns (id);
    bproc_free (id);
    free_iopages (id);
    free_drivers (id);
    if (PROC_IS_RUNNING(id))
        DRM(procs, running, id);
    else
        DRM(procs, sleeping, id);
    LPUSH(procs, zombie, id);
    proc[id].flags = PROC_ZOMBIE;
}

void
resume_waiting (id)
{
    char w;
    if (w = procs[id].first_wait)
        resume (procs[id].waiting[w]);
    schedule ();
}

char
wait (char id)
{
    char w;

    if (!proc[id])
        ERROR();

    ALLOC_WAITING(id, w);
    procs[id].waiting[w].pid = pid;

    if (!proc_flags[id])
        guru_meditation ();
    if (PROC_IS_ZOMBIE(id)) {
        do {
            suspend (id);
            schedule ();
        } while (PROC_IS_ZOMBIE(id));
    }
    return end_wait (id, w);
}

char
end_wait (char id, char w)
{
    char f;
    RM_WAITING(id, w);
    if (f = procs[id].first_wait) {
        resume (procs[id].waiting[f].pid);
        return exit_codes[id];
    }
    RM_ZOMBIE(id);
    procs[id].flags = 0;
    return exit_codes[id];
}

void
exit (char code)
{
    kill (pid, code);
}

void
kill (char id, char code)
{
    exit_codes[id] = code;
    if (!procs[id].flags)
        ERROR();
    zombify (id);
    resume_waiting ();
}
