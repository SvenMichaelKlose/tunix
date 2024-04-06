// TUNIX virtual console
//
// Open console menu when C= key has
// been released without combination.

#include <lib/tunix/tunix.h>

char active;
char menu_pid;
char consoles[8];

void
install_iopage (void)
{
}

void
init_roms (void)
{
}

void
start_basic (void)
{
}

void
link_iopage (void)
{
}

void
draw_menu (void)
{
}

void
select_console (char i)
{
    tunix_suspend (consoles[active]);
    active = i;
    tunix_resume (consoles[active]);
}

// Launch new BASIC instance on a
// virtual console.
void
launch (char i)
{
    char pid = tunix_fork ();
    if (pid) {
        consoles[i] = pid;
        select_console (i);
    } else {
        init_roms ();
        link_iopage ();
        start_basic ();
    }
}

// IO page installed.
void
interrupt_handler ()
{
    //if (CBM_KEY())
        tunix_resume (menu_pid);
}

void
menu (void)
{
    //char c;

    while (1) {
        tunix_suspend (consoles[active]);
        draw_menu ();
//        c = key_in ();
    }
}

int
main (int argc, char * argv[])
{
    (void) argc;
    (void) argv;
    install_iopage ();
    menu_pid = tunix_getpid ();
    menu ();
}
