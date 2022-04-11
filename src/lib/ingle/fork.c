char
fork ()
{
    char pid;

    cbm_open (31, 31, 15, "PF");
    cbm_chkout (31);
    pid = cbm_basin ();
    cbm_close (31);

    return pid;
}
