#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <simpleio/control.h>
#include <lisp/liblisp.h>

jmp_buf restart_point;

extern void test (void);

// Symbols for quoting.
lispptr quote;
lispptr quasiquote;
lispptr unquote;
lispptr unquote_spliced;

#ifdef __CC65__
#pragma rodata-name (push,"RODATA_INIT")
#endif

const char * env_files[] = {
#ifdef TEST
    "smoke-test-read.lisp",
#endif
    "git-version.lisp",
    "env-0.lisp",
#ifdef TEST
    "smoke-test.lisp",
#endif
    "env-1.lisp",

    // Target-specific
#if defined(TARGET_C128) || defined(TARGET_C16) || defined(TARGET_C64) || defined(TARGET_PET) || defined(TARGET_PLUS4) || defined(TARGET_VIC20)
    "cbm-common.lisp",
#endif
#ifdef TARGET_UNIX
    "unix.lisp",
#endif

#ifdef TEST
    "test.lisp",
#endif
    "env-2.lisp",
#ifndef NO_ONERROR
#ifdef TEST
    "test-onerror.lisp",
#endif
#endif

    // Early end for small machines.
    // TODO: More generic name than TARGET_C16.
#ifdef TARGET_C16
    "welcome.lisp",
#endif

#ifndef TARGET_C16
    "autoload.lisp",
#ifdef TEST
    "test-file.lisp",
#endif
#ifdef TEST
    "test-all.lisp",
#endif
    "welcome.lisp",
#endif // #ifndef TARGET_C16
    NULL
};

#ifdef __CC65__
#pragma rodata-name (pop)
#endif

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#endif

void
init_quoting (void)
{
    // Make symbols for quoting.
    quote           = make_symbol ("quote", 5);
    expand_universe (quote);
    quasiquote      = make_symbol ("quasiquote", 10);
    expand_universe (quasiquote);
    unquote         = make_symbol ("unquote", 7);
    expand_universe (unquote);
    unquote_spliced = make_symbol ("unquote-spliced", 15);
    expand_universe (unquote_spliced);
}

void
init_io_symbols (void)
{
    lispptr i;
    lispptr o;

    // Make symbols to hold current I/O channel numbers.
    fnin  = STDIN;
    fnout = STDOUT;
    i = make_number (STDIN);
    o = make_number (STDOUT);

    tmp  = make_symbol ("stdin", 5);
    SET_SYMBOL_VALUE(tmp, i);
    expand_universe (tmp);

    tmp = make_symbol ("stdout", 6);
    SET_SYMBOL_VALUE(tmp, o);
    expand_universe (tmp);

    lisp_fnin  = make_symbol ("fnin", 4);
    SET_SYMBOL_VALUE(lisp_fnin, i);
    expand_universe (lisp_fnin);

    lisp_fnout = make_symbol ("fnout", 5);
    SET_SYMBOL_VALUE(lisp_fnout, o);
    expand_universe (lisp_fnout);
}

extern void test (void);

void
lisp_init (void)
{
#ifdef TARGET_UNIX
    bekloppies_start = bekloppies ();
#endif

    // Init components.
    simpleio_init ();
    if (!init_heap ()) {
        outs ("No memory.");
        exit (EXIT_FAILURE);
    }
#if defined(TEST) && defined(TARGET_UNIX)
    test ();
#endif
    init_list ();
    init_eval ();
    init_builtins ();
    init_quoting ();
    init_io_symbols ();
    init_repl ();

#ifdef GC_STRESS
    do_gc_stress = true;
#endif
}

#ifdef __CC65__
#pragma code-name ("CODE")
#endif

int
main (int argc, char * argv[])
{
    const char ** f;
#ifndef NO_IMAGES
    lispptr istart_fun;
#endif

    // Muffle compiler warnings.
    (void) argc, (void) argv;

    lisp_init ();
#ifdef WAS_TARGET_VIC20
    heap_add_init_areas ();
#endif

#ifndef NO_IMAGES
    if (!setjmp (restart_point)) {
        // Try to load image.
        strcpy (buffer, "image");
        if (image_load (buffer))
            longjmp (restart_point, 1);
#endif // #ifndef NO_IMAGES

        // Load environment files.
        for (f = env_files; *f; f++)
            load ((char *) *f);
#ifndef NO_IMAGES
    } else {
        // Called from ILOAD.  Reset I/O.
        simpleio_init ();

        // Call function ISTART.
        istart_fun = make_symbol ("istart", 6);
        if (CONSP(SYMBOL_VALUE(istart_fun))) {
           PUSH(istart_fun);
           x = make_cons (istart_fun, nil);
           stack += sizeof (lispptr);
           (void) eval ();
        }
    }
#endif // #ifndef NO_IMAGES

    lisp_repl (REPL_STD);

    setout (STDOUT);
    outs ("Bye!");
    return 0;
}
