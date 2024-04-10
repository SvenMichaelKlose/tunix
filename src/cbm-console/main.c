// TUNIX virtual console

#include <lib/tunix/tunix.h>

extern char iopage;
extern char active;
extern char menu_pid;
extern void install_interrupt_handler (void);

char menu_pid;
char iopage;

char consoles[8];
char active;

void
start_basic (void)
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
        start_basic ();
    }
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

    cputs ("VIRTUAL CONSOLE\n");
    menu_pid = tunix_getpid ();
    iopage = tunix_iopage_alloc ();
    //if (!iopage)
    //    error ("OUT OF MEMORY FOR I/O PAGE");
    install_interrupt_handler ();
    menu ();
}
