#include <stdio.h>

#include "bdb.h"
#include "cache.h"
#include "tree2dot.h"

void
tree2dot_node (FILE *out, bdb_iter *iter, void *n)
{
    char * label;
    if (!n)
        return;

    label = iter->string (n);
    fprintf (out, "n%x [label=\"%s\"];\n", iter->id (n), label);
    if (iter->left (n)) {
        fprintf (out, "n%x -> n%x [label=\"L\"];\n", iter->id (n), iter->id (iter->left (n)));
        tree2dot_node (out, iter, iter->left (n));
    }
    if (iter->right (n)) {
        fprintf (out, "n%x -> n%x [label=\"R\"];\n", iter->id (n), iter->id (iter->right (n)));
        tree2dot_node (out, iter, iter->right (n));
    }
}

void
tree2dot (FILE *out, bdb_iter *iter, void *n)
{
    fprintf (out, "digraph BinaryTree {\n"
                  "node [shape=circle, style=filled, color=lightblue, fontcolor=black, fontsize=12];\n"
                  "edge [arrowsize=0.8];\n");
    tree2dot_node (out, iter, n);
    fprintf (out, "}\n");
}
