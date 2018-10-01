#define MAX_LINE_LENGTH 255
#define MAX_PARAMS 16

#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
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
    return errno;
}

int
ls (char ** values)
{
    struct g_dirent dirent;
    int dir;
    size_t bytes_read;

    if (!(dir = open ("/", 0)))
        return print_error ();

    while (1) {
        if (!(bytes_read = read (dir, &dirent, sizeof (struct g_dirent)))) {
            if (errno)
                return print_error ();
            break;
        }
        printf ("%s\n", &dirent.name);
    }
    close (dir);

    return 0;
}

/*
int
cat_single (char * path)
{
    char buffer[128];
    FILE * file;
    size_t bytes_read;

    if (!(file = fopen (path, "r")))
        return print_error ();

    while (!feof (file)) {
        if (!(bytes_read = fread (&buffer, sizeof (buffer), 1, file))) {
            if (errno)
                return print_error ();
            break;
        }
        fwrite (&buffer, sizeof (buffer), 1, stdout);
    }
    fclose (file);

    return 0;
}

int
cat (char ** values)
{
    int r;

    while (*values)
        if (r = cat_single (*values++))
            return r;

    return 0;
}
*/

typedef int (*command_function) (char **);

struct command {
    char * name;
    command_function fun;
} commands[] = {
    { "echo", echo },
    { "ls", ls },
/*
    { "cat", cat },
*/
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
