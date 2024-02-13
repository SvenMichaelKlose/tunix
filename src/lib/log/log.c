#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include <lib/ultimem/ultimem.h>

#include "liblog.h"

#ifndef LOG_START
#define LOG_START 0x0400
#endif

char log_bank = 116;

void
init_log_message ()
{
    *((char **) LOG_START) = (char *) LOG_START + sizeof (char *);
}

void
log_message (char * format, ...)
{
    va_list args;
    char * log_ptr;
    //char oldbank = *ULTIMEM_BLK5;
    //*ULTIMEM_BLK5 = log_bank;

    va_start(args, format);
    log_ptr = *(char **) LOG_START;
    vsprintf (log_ptr, format, args);
    log_ptr += strlen (log_ptr) + 1;
    *(char **) LOG_START = log_ptr;
    va_end(args);

    //*ULTIMEM_BLK5 = oldbank;
}
