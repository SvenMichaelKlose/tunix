// TUNIX virtual console

#include <lib/tunix/tunix.h>
#include <conio.h>
#include <stdio.h>

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

extern int main (int argc, char * argv[]);
int
main (int argc, char * argv[])
{
    (void) argc;
    (void) argv;

    *(char *) 0x900f = 0x1b;
    menu_pid = tunix_getpid ();
    iopage = tunix_iopage_alloc ();
    printf ("CBM vconsole\n");
    printf ("I/O page is %02x.\n", iopage);
    if (!iopage) {
        printf ("Out of memory for I/O page.\n");
        return -1;
    }
    install_interrupt_handler ();
    menu ();
}
