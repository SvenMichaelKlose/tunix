#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define IMAGE_BLOCKS    (8192 / 64)

void
copy_file (int out, char * path)
{
    char * data;
    int in;

    printf ("<- %s\n", path);
    data = malloc (65536);
    memset (data, 255, 65536);
    if ((in = open (path, O_RDONLY)) < 0)
        perror ("open in");
    if (!read (in, data, 65536))
        perror ("read");
    if (!write (out, data, 65536))
        perror ("write");
    close (in);
}

int
main (int argv, char ** argc)
{
    int i;
    int out;
    char * data = malloc (65536);
    memset (data, 255, 65536);

    if ((out = open ("compiled/ultimem.img", O_CREAT | O_WRONLY | O_TRUNC, 0644)) < 0)
        perror ("open out");

    copy_file (out, "src/flashboot/flashboot.bin");
    copy_file (out, "src/flashmenu/flashmenu.bin");
    for (i = 0; i < IMAGE_BLOCKS - 2; i++)
        if (!write (out, data, 65536))
            perror ("write");

    close (out);

    return 0;
}
