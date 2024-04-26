#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1024

char buffer[BUFFER_SIZE];
char *ifname;
FILE *in;
char ofname[256];

int
extract_file (int ntotal)
{
    FILE *out = fopen (ofname, "wb");
    int len;

    printf ("Extracting file '%s' ");
    if (!out) {
        perror ("Failed to create output file");
        return -1;
    }
    while (ntotal
           && (len = fread (buffer,
                            1,
                            ntotal > BUFFER_SIZE ?
                                BUFFER_SIZE :
                                ntotal,
                            in)) > 0) {
        fwrite (buffer, 1, len, out);
        ntotal -= len;
    }
    fclose (out);
    printf ("OK\n");
    return 0;
}

int
main (int argc, char *argv[])
{
    int filesize, n;

    if (argc != 2) {
        printf ("Usage: %s <arc_file>\n", argv[0]);
        return -1;
    }

    in = fopen (argv[1], "rb");
    if (!in) {
        perror ("Failed to open file.");
        return -1;
    }

    while (n = fscanf (in, ">>> %255s %d\n", ofname, &filesize)) {
        if (n == EOF)
            break;
        if (n != 2) {
            perror ("Corrupt file signature.");
            return -1;
        }
        if (extract_file (filesize))
            return -1;
    }

    fclose (in);
    return 0;
}
