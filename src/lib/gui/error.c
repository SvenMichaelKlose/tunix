#include <lib/gui/message.h>
#include <lib/gui/error.h>

void __fastcall__
print_error (char * text)
{
    print_message (text);
    while (1);
}

void
error_out_of_heap_memory ()
{
    print_error ("Error: Out of heap memory.");
}
