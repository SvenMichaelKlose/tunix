#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "log.h"
#include "lib/ultimem/ultimem.h"

char * log_ptr = (char *) 0xa000;

char log_bank = 116;

void
log_message (char * format, ...)
{
    va_list args;
    char oldbank = *ULTIMEM_BLK5;
    *ULTIMEM_BLK5 = log_bank;

    va_start(args, format);
    vsprintf (log_ptr, format, args);
    log_ptr += strlen (log_ptr) + 1;
    va_end(args);

    *ULTIMEM_BLK5 = oldbank;
}
