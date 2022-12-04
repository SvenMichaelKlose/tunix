char
fork ()
{
    char pid;

    cbm_k_open (31, 31, 15, "PF");
    cbm_k_chkin (31);
    pid = cbm_k_basin ();
    cbm_k_close (31);

    return pid;
}

void
kill (char pid)
{
    char err;
    char cmd[4];

    cmd[0] = 'P';
    cmd[1] = 'K';
    cmd[2] = pid;
    cmd[3] = 0;
    cbm_k_open (31, 31, 15, "PK");
    cbm_k_chkin (31);
    err = cbm_k_basin ();
    cbm_k_close (31);

    return err;
}
