#include <libterm.h>
#include <cbm.h>

extern char get_key (void);

int
main ()
{
    char key;

    term_init ();

    while (1) {
        while (!(key = get_key ()));
        term_put (key);
    }
}
