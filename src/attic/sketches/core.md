The core
========

The core dispatches KERNAL calls to registered drivers and to itself.
This improves compatibility and speed of drivers and reduces memory
consumption.  It also handles multi-tasking.  Up to 32 processes (including
drivers) can be served at a time.

# Commodore KERNAL overview

Explain the API, LFNs, devices and secondary addresses.

# KERNAL hooks

The core places its hooks in the IO2,3 area ranging from $9800 to $9fff
along with dispatch tables and buffers to do fast block reads and writes.
To support multi-tasking, the LFNs of a process are translated to global LFNs
individually before they are passed to a driver.

## IO2,3

The IO2,3 area starts with information relevant to drivers,
followed by the hooks which are subject to change anytime.

| Range     | Use              |
--------------------------------
| 9800-98ff | Global LFNs      |
| 9a00-9a00 | Process ID       |
| 9a01-9a04 | CPU registers    |
| 9a05-9a06 | RAM1,2,3 bank    |
| 9a07-9a08 | BLK1 bank        |
| 9a09-9a0a | BLK2 bank        |
| 9a0b-9a0c | BLK3 bank        |
| 9a0d-9a0e | IO2,3 bank       |
| 9a0f-9a10 | BLK5 bank        |
| 9a11-9fff | Hooks + data     |
--------------------------------

# Drivers

Drivers are programs that register themselves as drivers via
the core device at #31.

~~~C
void
register_as_driver ()
{
    char command[256];
    sprintf (command,
             "dr:"

             "open:%#04X,"  "close:%#04X," "clall:%#04X,"
             "chkin:%#04X," "ckout:%#04X,"
             "basin:%#04X," "getin:%#04X," "bsout:%#04X,"
             "load:%#04X,"  "save:%#04X."
             "readb:%#04X," "writeb:%#04X,"
             "\n",

             our_open,  our_close, our_clall
             our_chkin, our_chkout,
             our_basin, our_getin, our_bsout,
             our_load,  our_save,
             our_readb, our_writeb,
             0x7fff);
    ctrl = cbm_k_open (15,31,15, command);
    cbm_k_chkin (31);
    r1 = cbm_k_basin ();
    r2 = cbm_k_basin ();
    if (r1 == '0' && r2 == '0')
        printf ("Driver installed.");
    else {
        printf ("Error!");
        while (!cbm_k_getst ())
            putc (cbm_k_basin ());
    }
    close (31);
}
~~~

| Parameter                       | Description             |
|---------------------------------|-------------------------|
| "open:%#04X,"  "close:%#04X,"   | Regular KERNAL vectors. |
| "clrcn:%#04X," "usr:%#04X"      |                         |
| "clall:%#04X," "chkin:%#04X,"   |                         |
| "ckout:%#04X," "basin:%#04X,"   |                         |
| "getin:%#04X," "basout:%#04X,"  |                         |
| "load:%#04X,"  "save:%#04X"     |                         |
|---------------------------------|-------------------------|
| "readb:%#04X," "writeb:%#04X"   | Block read/write.       |
|---------------------------------|-------------------------|
 
