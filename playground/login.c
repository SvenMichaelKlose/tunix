char  user[9];
char  pass[9];
char  u[9];
char  p[9];
char  cmd[64];
char  id;
    
void
main (void)
{
    while (1) {
        printf ("User: ");
        inputline (&user, 8, true);
        printf ("Password: ");
        inputline (&pass, 8, false);

        f = fopen ("/etc/passwd");
        if (f < 0) {
            printf ("No /etc/passwd.\n");
            fclose (f);
            execute ("/bin/sh");
            /* NOTREACHED */
        }
        while (fscanf (f, "%s:%s:%d:%s\n", &u, &p, &id, &cmd))
            if (!strcmp (user, u) && !strcmp (pass, p)) {
                fclose (f);
                setuser (id);
                execute (cmd);
                /* NOTREACHED */
            }
        fclose (f);
    }
}
