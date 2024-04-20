// Just some code to test new targets.

char array[5 + 3];

main (char argc, char *argv[])
{
    char i;
    char *p;
    p = 0x1000;
    for (i = 0; i < 500; i++)
        p[i] = 23;
}
