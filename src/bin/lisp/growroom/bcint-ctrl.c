// Most simple bytecode interpreter, dealing with control flow only.

void
run_sbc (char * x)
{
    char * p = s;

    while (ok) {
        switch (*p) {
        case BC_LIST:
            p++;
            c = &s[*(unsigned *) p];
            p++;
            while (c--) {
                value = eval (*(lispptr *) p);
                p += sizeof (lispptr);
            }
            continue;

        case BC_GO:
            goto jump;

        case BC_GO_NIL:
            if (NOT(value))
                goto jump;
            goto skip_jump;

        case BC_GO_NNIL:
            if (NOT_NIL(value))
                goto jump;
            goto skip_jump;

        case BC_END:
            break;

#ifndef NDEBUG
        default:
            internal_error ("ill bc");
#endif
        }
jump:
        p = &s[*(unsigned *) (p + 1)];
        continue;

skip_jump:
        p += 1 + sizeof (unsigned);
    }
}
