extern char *brkend;

sbrk (char *incr)
{
    char *stktop;

    stktop = Xstktop () - 200;
    if (brkend + incr < stktop) {
        stktop = brkend;
        brkend = brkend + incr;
        return stktop;
    } else
        return -1;
}
