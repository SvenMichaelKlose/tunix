#define MAX_LINE_LENGTH 255
#define MAX_PARAMS 16

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#define CHAR_RETURN     10
#define CHAR_BACKSPACE  8

void
print_prompt (FILE * out)
{
    fprintf (out, "$ ");
}

int
get_line (FILE * out, FILE * in, char * line)
{
    int count = 0;
    char c;

    print_prompt (out);

    while (1) {
        c = fgetc (in);
        if (c == CHAR_RETURN) {
            line[count++] = 0;
            fputc ('\n', out);
            return count;
        }
        if (c == CHAR_BACKSPACE) {
            if (count) {
                line[--count] = 0;
                fputc (c, out);
            }
            continue;
        }
        if (count == MAX_LINE_LENGTH)
            continue;
        line[count++] = c;
        fputc (c, out);
    }
}

int
parse (char ** values, char * in)
{
    char * tmp;
    char * out;
    size_t len;

    *values = NULL;
    while (*in == ' ')
        in++;
    if (!*in)
        return 0;
    tmp = in;
    len = 0;
    while (*tmp++ > ' ')
        len++;
    *values = out = malloc (len + 1);
    while (*in > ' ')
        *out++ = *in++;
    *out = 0;
    return 1 + parse (++values, in);
}

void
free_values (char ** values)
{
    int i;
    for (i = 0; values[i]; i++)
        free (values[i]);
}

int
echo (char ** values)
{
    int i;

    for (i = 1; values[i]; i++) {
        if (i > 1)
            fputc (' ', stdout);
        fputs (values[i], stdout);
    }
    fputc ('\n', stdout);

    return 0;
}

struct g_dirent {
    char name[16];
    long size;
    char type;
};

int
print_error ()
{
    fprintf (stderr, "sh: %s (%d)\n", strerror (errno), errno);
    return -1;
}

int
ls (char ** values)
{
    struct g_dirent dirent;
    FILE * dir;
    size_t bytes_read;

    if (!(dir = fopen ("/", "r")))
        return print_error ();

    while (!feof (dir)) {
        if (!(bytes_read = fread (&dirent, sizeof (struct g_dirent), 1, dir))) {
            if (errno)
                return print_error ();
            break;
        }
        printf ("%s\n", &dirent.name);
    }
    fclose (dir);

    return 0;
}

typedef int (*command_function) (char **);

struct command {
    char * name;
    command_function fun;
} commands[] = {
    { "echo", echo },
    { "ls", ls },
    { NULL, NULL }
};

command_function
find_command (char * name)
{
    int i;

    for (i = 0; commands[i].name; i++)
        if (!strcmp (commands[i].name, name))
            return commands[i].fun;
    return NULL;
}

int
main (char ** argv, int argc)
{ 
    char * line = malloc (MAX_LINE_LENGTH + 1);
    char * values[MAX_PARAMS];
    command_function f;

    while (1) {
        get_line (stdout, stdin, line);
        if (!parse (values, line))
            continue;
        if (!strcmp ("exit", values[0]))
            break;
        if (f = find_command (values[0])) {
            f (values);
            continue;
        }
        printf ("%s: command not found\n", values[0]);
    }

    return 0;
}
