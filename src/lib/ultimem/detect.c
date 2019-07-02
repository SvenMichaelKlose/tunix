#include "ultimem.h"

char
ultimem_is_installed ()
{
    char id, dev;

    ultimem_send_command (0x90);
    id = *((char *) 0xa000);
    dev = *((char *) 0xa002);
    ultimem_send_command (0xf0);

    return id == 1 && dev == 0x7e;
}

unsigned
ultimem_get_size ()
{
    unsigned short s;

    ultimem_send_command (0x90);
    s = *((char *) 0xa01c) << 8 + *((char *) 0xa01e);
    ultimem_send_command (0xf0);

    return s;
}
