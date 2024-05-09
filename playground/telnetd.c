int
main (void)
{
    socket (2, 22);
    chkin (2);
    while ((s = basin ())) {
        if (!pid = fork ()) {
            fn2dev (0, s);
            fn2dev (3, s);
            exec ("/bin/login");
            return 0;
        }
        wait (pid);
    }
    return 0;
}
