#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "defs.h"
#include "data.h"
#include "ir.h"

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

main (int argc, char *argv[])
{
    char *param = NULL, *bp;
    int smacptr, i;
    macptr = 0;
    ctext = 0;
    errs = 0;
    aflag = 1;
#ifdef I8080
    uflag = 0;
#endif

    for (i = 1; i < argc; i++) {
        param = argv[i];
        if (*param == '-') {
            while (*++param) {
                switch (*param) {
                // output c source as asm comments 
                case 't':
                case 'T':
                    ctext = 1;
                    break;
                // no argument count in A to function calls 
                case 'a':
                case 'A':
                    aflag = 0;
                    break;
                // use undocumented 8085 instructions 
                case 'u':
                case 'U':
#ifdef I8080
                    uflag = 1;
#endif
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
                // set the log if one wishes 
                case 'l':
                case 'L':
                    if (logFile) {
                        fclose (logFile);
                        logFile = NULL;
                    }

                    if (logFile = fopen (++param, "rb")) {
                        fclose (logFile);
                        logFile = NULL;

                        oputs ("Appending log to ");
                        oputs (param);
                        oputs (".\n");
                    }

                    if (!(logFile = fopen (param, "ab")))
                        perror ("CAN'T OPEN LOGFILE");
                    break;
                default:
                    usage ();
                }
            }
        } else
            break;
    }

    // command line defined macros -d 
    smacptr = macptr;

    if (!param)
        usage ();

    for (; i < argc; i++) {
        param = argv[i];
        errfile = 0;
        macptr = smacptr;
        compile (param);
    }
    exit (!!errs);
}

// Compile one file if filename is NULL
// redirect do to stdin/stdout.
compile (char *file)
{
    // copy actual filename to filename array.
    strcpy (finame[0], file);
    // reset source line counter 
    srcln[0] = 0;
    if (file == NULL || filename_typeof (file) == 'c') {
        global_table_index = 0;
        local_table_index = NUMBER_OF_GLOBALS;
        while_table_index = 0;
        tag_table_index = 0;
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
        if (file == NULL) {
            input = stdin;
        } else if (!openin (file))
            return;
        if (file == NULL) {
            output = stdout;
        } else if (!openout ())
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
    } else {
        oputs ("Don't understand file ");
        oputs (file);
        errs = 1;
    }
}

frontend_version ()
{
    outs ("; Front End (3.2,24/04/21)");
}

usage ()
{
    oputs
        ("usage: scc [-tcsah] [-dSYM[=VALUE]] [-l[log]] files\n");
    oputs ("-t: output c source as asm comments\n");
    oputs
        ("-a: no argument count in A to function calls\n");
    oputs ("-d: define macro\n");
    oputs
        ("-u: use undocumented 8085 instructions LDSI, LHLX, SHLX\n");
    oputs
        ("-s: assemble generated output, not implemented\n");
    oputs ("-c: link, not implemented\n");
    oputs ("-h: displays usage\n");
    oputs ("-l: set the log\n");
    oputs
        ("log - a file that you wish to contain most (if not all) messages\n");
    oputs
        ("files - one or more files. no filename redirects to stdin/stdout\n");
    exit (1);
}

// Process all input text.
// At this level, only static
// declarations, defines, includes,
// and function definitions are legal.
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
    } else if (type = get_type ())
        declare_global (type, stclass, mtag, NULL_TAG, is_struct);
    else if (stclass == PUBLIC)
        return 0;
    else
        declare_global (CINT, stclass, mtag, NULL_TAG, is_struct);
    need_semicolon ();
    return 1;
}

// Dump ppol of literals.
dumplits ()
{
    int j, k;

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
dumpglbs ()
{
    int dim, i, list_size, line_count, value;

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
                    // has initials 
                    list_size = get_size (symbol->name);
                    if (dim == -1)
                        dim = list_size;
                }
                for (i = 0; i < dim; i++) {
                    if (symbol->type == STRUCT) {
                        dump_struct (symbol, i);
                    } else {
                        if (!(line_count % 10)) {
                            newline ();
                            if (symbol->type & CINT
                                || symbol->identity == POINTER) {
                                gen_dataw ();
                            } else
                                gen_datab ();
                        }
                        if (i < list_size) {
                            // dump data 
                            value = get_item_at (symbol->name, i, &tag_table [symbol-> tagidx]);
                            outw (value);
                        } else
                            // Dump zero, no more data available.
                            outw (0);
                        line_count++;
                        if (!(line_count % 10))
                            line_count = 0;
                    }
                }
                newline ();
            }
        } else
            gen_decl_fun (symbol);
        current_symbol_table_idx++;
    }
}

// Dump struct data.
dump_struct (SYMBOL * symbol, int position)
{
    int i, number_of_members, value;
    number_of_members = tag_table[symbol->tagidx].number_of_members;
    newline ();
    for (i = 0; i < number_of_members; i++) {
        // i is the index of current member, get type 
        SYMBOL member = member_table[tag_table[symbol->tagidx].  member_idx + i];
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
                value = get_item_at (symbol->name, position * number_of_members + i, &tag_table[symbol-> tagidx]);
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

errorsummary ()
{
    if (ncmp)
        error ("missing closing bracket");
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
filename_typeof (char *s)
{
    s += strlen (s) - 2;
    if (*s == '.')
        return *(s + 1);
    return ' ';
}
