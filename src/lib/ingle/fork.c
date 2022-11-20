char
fork ()
{
    char pid;

    cbm_k_open (31, 31, 15, "PF");
    cbm_k_ckout (31);
    pid = cbm_k_basin ();
    cbm_k_close (31);

    return pid;
}
