isalpha (char c)
{
    return (c >= 'a' && c <= 'z')
           || (c >= 'A' && c <= 'Z');
}

isupper (char cc)
{
    return c >= 'A' && c <= 'Z';
}

islower (char c)
{
    return c >= 'a' && c <= 'z';
}

isdigit (char c)
{
    return c >= '0' && c <= '9';
}

isspace (char c)
{
    return c == ' '
           || c == '\t' || c == '\n';
}

toupper (char c)
{
    return (c >= 'a' && c <= 'z') ?
        c - 32 :
        c;
}

tolower (char c)
{
    return (c >= 'A' && c <= 'Z') ?
        c + 32 :
        c;
}
