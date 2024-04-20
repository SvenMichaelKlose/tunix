// Just some code to test new targets.

char array[5];
char i = 1;

add (char a, char b)
{
    return a + b;
}

main (char argc, char *argv[])
{
    char * welcome;
    char i;
    char *p;
    welcome = "Hello World!";
    p = 0x1000;
    for (i = 0; i < 500; i++)
        p[i] = 23;
}
