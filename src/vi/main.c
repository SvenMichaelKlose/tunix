#include <libterm.h>
#include <cbm.h>

int
main ()
{
    char key;

    term_init ();
    term_put (65);

    while (1) {
        while (!(key = cbm_k_getin ()));
        term_put (key);
    }
}
