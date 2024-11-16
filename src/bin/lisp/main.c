#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <simpleio/control.h>
#include <lisp/liblisp.h>

#ifdef TARGET_CPM
long heap;
#endif

jmp_buf restart_point;

extern void test (void);

// Symbols for quoting.
lispptr quote;
#ifndef NO_QUASIQUOTE
lispptr quasiquote;
lispptr unquote;
lispptr unquote_spliced;
#endif

#ifdef __CC65__
#pragma rodata-name (push,"RODATA_INIT")
#endif

const char * env_files[] = {
#ifdef TEST_ENVIRONMENT
    "smoke-test-read.lsp",
#endif
    "git-version.lsp",
    "env-0.lsp",
#ifdef TEST_ENVIRONMENT
    "smoke-test.lsp",
#endif
    "equality.lsp",
    "list.lsp",

    // Target-specific
#if defined(TARGET_C128) || defined(TARGET_C16) || defined(TARGET_C64) || defined(TARGET_PET) || defined(TARGET_PLUS4) || defined(TARGET_VIC20)
    "cbm-common.lsp",
#endif
#ifdef TARGET_UNIX
    "unix.lsp",
#endif

#ifdef TEST_ENVIRONMENT
    "test.lsp",
#endif
#ifndef NO_QUASIQUOTE
    "quasiquote.lsp",
    #ifdef TEST_ENVIRONMENT
        "test-qq.lsp",
    #endif
#endif
#ifndef NO_MACROEXPAND
    "macroexpand.lsp",
    #ifdef TEST_ENVIRONMENT
        "test-macros.lsp",
    #endif
#endif
#ifndef NO_ONERROR
#ifdef TEST_ENVIRONMENT
    "test-error.lsp",
#endif
#endif

    // Early end for small machines.
    // TODO: More generic name than TARGET_C16.
#ifdef TARGET_C16
    "welcome.lsp",
#endif

#ifndef TARGET_C16
    "autoload.lsp",
    "reset!.lsp",
#if defined(TEST_ENVIRONMENT)
    "test-autoload.lsp",
#endif
#if defined(TEST_ENVIRONMENT) && !defined(NO_BUILTIN_GROUP_FILE)
    "test-file.lsp",
#endif
    "welcome.lsp",
#endif // #ifndef TARGET_C16
#ifdef TEST_ALL
    "test-all.lsp",
#endif // #ifdef TEST_ALL
    NULL
};

#ifdef __CC65__
#pragma rodata-name (pop)
#endif

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#endif

void
init_quote_symbols (void)
{
    quote           = make_symbol ("quote", 5);
    expand_universe (quote);
#ifndef NO_QUASIQUOTE
    quasiquote      = make_symbol ("quasiquote", 10);
    expand_universe (quasiquote);
    unquote         = make_symbol ("unquote", 7);
    expand_universe (unquote);
    unquote_spliced = make_symbol ("unquote-spliced", 15);
    expand_universe (unquote_spliced);
#endif
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

    simpleio_init ();
    if (!init_heap ()) {
        outs ("No memory.");
        exit (EXIT_FAILURE);
    }
#if defined(TEST_INTERPRETER) && defined(TARGET_UNIX)
    test ();
#endif
    init_list ();
    init_eval ();
    init_builtins ();
    init_quote_symbols ();
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
#ifndef NO_BUILTIN_LOAD
    const char ** f;
#endif
#ifndef NO_IMAGE
    lispptr istart_fun;
#endif

    // Muffle compiler warnings.
    (void) argc, (void) argv;

#if defined(DEVELOPMENT) && defined(__CC65__)
    memset ((char *) 0x100, 0, 0xe0);
#endif
    lisp_init ();
#ifdef WAS_TARGET_VIC20
    heap_add_init_areas ();
#endif

#ifndef NO_IMAGE
    if (!setjmp (restart_point)) {
        // Try to load image.
        strcpy (buffer, "image");
        if (image_load (buffer))
            longjmp (restart_point, 1);
#endif // #ifndef NO_IMAGE

#ifndef NO_BUILTIN_LOAD
        // Load environment files.
        for (f = env_files; *f; f++)
            load ((char *) *f);
#endif
#ifndef NO_IMAGE
    } else {
        // Called by ILOAD.  Reset I/O.
        simpleio_init ();
#ifndef NO_BUILTIN_GROUP_FILE
        SET_SYMBOL_VALUE(lisp_fnin,  make_number (STDIN));
        SET_SYMBOL_VALUE(lisp_fnout, make_number (STDOUT));
#endif

        // Call function ISTART.
        istart_fun = make_symbol ("istart", 6);
        if (CONSP(SYMBOL_VALUE(istart_fun))) {
           x = make_cons (istart_fun, nil);
           (void) eval ();
        }
    }
#endif // #ifndef NO_IMAGE

    lisp_repl (REPL_STD, 0);
    return 0;
}
