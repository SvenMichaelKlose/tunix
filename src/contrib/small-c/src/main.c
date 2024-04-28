// TODO: Output file name option '-o'.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "defs.h"
#include "data.h"
#include "ir-codes.h"
#include "io.h"
#include "gen.h"
#include "lex.h"
#include "ir.h"
#include "initials.h"
#include "sym.h"
#include "struct.h"
#include "function.h"
#include "stmt.h"
#include "preproc.h"
#include "main.h"

FILE *logFile = NULL;

// Global input filenames for error
// messages.
char finame[INCLSIZ + 1][20];

// Source file line counters for error
// messages.
int srcln[INCLSIZ + 1] = { 0, 0, 0, 0 };

// Simple oputs function to replace the
// ugly fputs(foo, stdout).
void
oputs (char *str)
{
    if (logFile)
        fputs (str, logFile);
    fputs (str, stderr);
}

int
main (int argc, char *argv[])
{
    char *param = NULL, *bp;
    int i;
    macptr = 0;
    ctext = 0;
    errs = 0;
#ifdef I8080
    uflag = 0;
#endif

    for (i = 1; i < argc; i++) {
        param = argv[i];
        if (*param == '-') {
            while (*++param) {
                switch (*param) {
                // Include source in object files.
                case 't':
                case 'T':
                    ctext = 1;
                    break;
                // define macro 
                case 'd':
                case 'D':
                    bp = ++param;
                    if (!*param)
                        usage ();
                    while (*param && *param != '=')
                        param++;
                    if (*param == '=')
                        *param = '\t';
                    while (*param)
                        param++;
                    param--;
                    defmac (bp);
                    break;
                default:
                    usage ();
                }
            }
        } else
            break;
    }

    if (!param || i < argc - 1)
        usage ();
    errfile = 0;
    compile (param);
    exit (!!errs);
}

// Compile one file if filename is NULL
// redirect do to stdin/stdout.
void
compile (char *file)
{
    // copy actual filename to filename array.
    strcpy (finame[0], file);
    // reset source line counter 
    srcln[0] = 0;
    if (file && filename_typeof (file) != 'c') {
        oputs ("File name with postfix '.c' expected.");
        oputs (file);
        errs = 1;
        return;
    }

    global_table_index = 0;
    local_table_index = NUMBER_OF_GLOBALS;
    while_table_index = 0;
    tag = 0;
    member = -1;
    inclsp =
        iflevel =
        skiplevel =
        swstp = litptr = stkp = errcnt = ncmp = lastst =
        0;
    input2 = NULL;
    cmode = 1;
    glbflag = 1;
    nxtlab = 0;
    litlab = getlabel ();
    defmac ("SMALLC 1");
    defmac ("short int");
    rglobal_table_index = global_table_index;
    initmac ();
    if (!file)
        input = stdin;
    else if (!openin (file))
        return;
    if (!file)
        output = stdout;
    else if (!openout ())
        return;
    header ();
    gen_code_segment ();
    parse ();
    fclose (input);
    gen_data_segment ();
    dumplits ();
    dumpglbs ();
    errorsummary ();
    trailer ();
    fclose (output);
    pl ("");
    errs = errs || errfile;
}

void
frontend_version ()
{
    outs ("; Front End (3.2,24/04/21)");
}

void
usage ()
{
    oputs ("Usage: scc [-th] [-dSYM[=VALUE]] input-file\n"
           "-d: Define macro.\n"
           "-t: Include C source in .o files.\n"
           "-h: Print this message and exit.\n"
           "input-file: Use standard I/O if none\n"
           "    has been specified.  Name must\n"
           "    end on suffix '.c'.  Output file\n"
           "    will have suffix '.o'.\n");
    exit (1);
}

// Process all input text.
// At this level, only static
// declarations, defines, includes,
// and function definitions are legal.
void
parse ()
{
    while (!feof (input)) {
        if (amatch ("extern", 6))
            do_declarations (EXTERN, NULL_TAG, 0);
        else if (amatch ("static", 6))
            do_declarations (STATIC, NULL_TAG, 0);
        else if (do_declarations (PUBLIC, NULL_TAG, 0));
        else if (match ("#asm"))
            doasm ();
        else if (match ("#include"))
            doinclude ();
        else if (match ("#define")) {
            dodefine ();
        } else if (match ("#undef"))
            doundef ();
        else
            newfunc ();
        blanks ();
    }
}

// Parse top level declarations.
int
do_declarations (int stclass,
                 TAG_SYMBOL * mtag,
                 int is_struct)
{
    int type;
    // Tag of struct object being
    // declared.
    int otag;
    // TRUE for struct definition,
    // FALSE for union.
    int sflag;
    char sname[NAMESIZE];

    blanks ();
    if ((sflag = amatch ("struct", 6))
        || amatch ("union", 5)) {
        if (!symname (sname))
            illname ();
        // Structure not previously
        // defined.
        if ((otag = find_tag (sname)) == -1)
            otag = define_struct (sname, stclass, sflag);
        declare_global (STRUCT, stclass, mtag, otag, is_struct);
    } else if ((type = get_type ()))
        declare_global (type, stclass, mtag, 0, is_struct);
    else if (stclass == PUBLIC)
        return 0;
    else
        declare_global (CINT, stclass, mtag, 0, is_struct);
    need_semicolon ();
    return 1;
}

// Dump ppol of literals.
void
dumplits ()
{
    int k;
    // TODO: Check what this achieves.
    if (!litptr)
        return;
    def_local (litlab);
    k = 0;
    outb (IR_DATAB);
    outw (litptr - k);
    while (k < litptr)
        outb (litq[k++] & 127);
}

// Dump static variables.
void
dumpglbs ()
{
    int dim, i, list_size, line_count;

    if (!glbflag)
        return;
    current_symbol_table_idx = rglobal_table_index;
    while (current_symbol_table_idx < global_table_index) {
        SYMBOL *symbol = &symbol_table[current_symbol_table_idx];
        if (symbol->identity != FUNCTION) {
            gen_decl_var (symbol);
            if (symbol->storage != EXTERN) {
                def_global (symbol->name);
                dim = symbol->offset;
                list_size = 0;
                line_count = 0;
                if (find_symbol_initials (symbol->name)) {
                    list_size = get_size (symbol->name);
                    if (dim == -1)
                        dim = list_size;
                }
                for (i = 0; i < dim; i++) {
                    if (symbol->type == STRUCT) {
                        dump_struct (symbol, i);
                    } else {
                        if (symbol->type & CINT
                            || symbol->identity == POINTER) {
                            gen_dataw ();
                        } else
                            gen_datab ();
                        if (i < list_size) {
                            // dump data 
                            outw (get_item_at (symbol->name, i, &tags [symbol-> tag]));
                        } else
                            // Dump zero, no more data available.
                            outw (0);
                        line_count++;
                        if (!(line_count % 10))
                            line_count = 0;
                    }
                }
            }
        } else
            gen_decl_fun (symbol);
        current_symbol_table_idx++;
    }
}

// Dump struct data.
void
dump_struct (SYMBOL * symbol, int position)
{
    int i, num_members, value;
    num_members = tags[symbol->tag].num_members;
    newline ();
    for (i = 0; i < num_members; i++) {
        // i is the index of current member, get type 
        SYMBOL member = members[tags[symbol->tag].first_member + i];
        // Array members need proper
        // storage space (the compiler
        // currently doesn't allow
        // arrays in structs to be
        // initilized.
        if (member.identity == ARRAY) {
            gen_bss ();
            outn (member.struct_size);
            newline ();
        } else {
            // Both pointers and ints
            // take two bytes.
            if (member.type & CINT || member.identity == POINTER)
                gen_dataw ();
            else
                gen_datab ();
            if (position < get_size (symbol->name)) {
                // dump data
                value = get_item_at (symbol->name, position * num_members + i, &tags[symbol-> tag]);
                outw (value);
            } else {
                // Dump zero, no more
                // data available.
                outw (0);
            }
            newline ();
        }
    }
}

void
errorsummary ()
{
    if (ncmp)
        perror ("missing closing bracket");
    return;

    // We're making IR code.
    gen_comment ();
    outn (errcnt);
    if (errcnt)
        errfile = YES;
    outs (" error(s) in compilation");
    newline ();
    outs ("; literal pool: ");
    outn (litptr);
    newline ();
    outs ("; global pool: ");
    outn (global_table_index - rglobal_table_index);
    newline ();
    outs ("; Macro pool: ");
    outn (macptr);
    newline ();
    if (errcnt > 0)
        pl ("Error(s)");
}

// Test for C or similar filename, e.g.
// xxxxx.x, tests the dot at end-1
// postion.
char
filename_typeof (char *s)
{
    s += strlen (s) - 2;
    if (*s == '.')
        return *(s + 1);
    return ' ';
}
