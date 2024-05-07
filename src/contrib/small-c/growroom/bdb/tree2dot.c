#include <stdio.h>

#include "bdb.h"
#include "cache.h"
#include "tree2dot.h"

void
tree2dot_node (FILE *out, cnode *n)
{
    if (!n)
        return;

    fprintf (out, "n%x;\n", n->id);
    if (n->kleft) {
        fprintf (out, "n%x -> n%x [label=\"l\"];\n", n->id, n->kleft->id);
        tree2dot_node (out, n->kleft);
    }
    if (n->kright) {
        fprintf (out, "n%x -> n%x [label=\"r\"];\n", n->id, n->kright->id);
        tree2dot_node (out, n->kright);
    }
}

void
tree2dot (FILE *out, cnode *n)
{
    fprintf (out, "digraph BinaryTree {\n"
                  "node [shape=circle, style=filled, color=lightblue, fontcolor=black, fontsize=12];\n"
                  "edge [arrowsize=0.8];\n");
    tree2dot_node (out, n);
    fprintf (out, "}\n");
}
