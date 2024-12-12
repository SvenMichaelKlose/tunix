(in-package 'as65
  '(*previous-labels* *next-labels* *imported-labels*
    *label-changed?* *mn-6502* *am-6502* *6502* *all-mnem65*
    pass files clear-labels rewind-labels update-label add-label
    get-label-in get-earlier-label get-later-label has-label?
    get-label-undirected get-label get-labels
    mn-cc adjust-am mnam-opc mnimm-opc opcode zpconv inst
    labeldef? mnem? parse))
