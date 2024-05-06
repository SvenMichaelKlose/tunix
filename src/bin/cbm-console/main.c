// TUNIX virtual console

#include <tunix/tunix.h>

#include <conio.h>
#include <stdlib.h>
#include <stdio.h>

extern char iopage;
extern char active;
extern char consoles[8];
extern char menu_pid;
extern void install_interrupt_handler (void);

char menu_pid;
char iopage;

char consoles[8];
char active;

void
draw_menu (void)
{
    clrscr ();
    printf ("Pick a console:");
}

// Launch new BASIC instance on a
// virtual console.
void
launch (char i)
{
    char pid = tunix_fork ();
    if (pid)
        consoles[i] = pid;
    else
        exit (0);
}

void
menu (void)
{
    char c;

    while (1) {
        tunix_suspend (menu_pid);
        if (!consoles[active])
            break;
        tunix_suspend (consoles[active]);
        draw_menu ();
        c = cbm_k_basin ();
        tunix_resume (consoles[active]);
    }

    printf ("CBM console exits.\n");
}

extern int main (int argc, char * argv[]);
int
main (int argc, char * argv[])
{
    (void) argc;
    (void) argv;

    printf ("TUNIX CBM console\n");
    menu_pid = tunix_getpid ();
    printf ("PID is $%02x.\n", menu_pid);
    iopage = tunix_iopage_alloc ();
    printf ("I/O page is $%02x.\n", iopage);
    if (!iopage) {
        printf ("Out of memory for I/O page.\n");
        return -1;
    }
    install_interrupt_handler ();
    launch (0);
    menu ();

    return 0;
}
