Ingle
=====

This is a document-oriented user interface made of windowed
terminals.  Documents are trees of auto-layouted graphics
primitives, provided in Lisp format:

A document is intruduced by the terminal escape sequence

~~~
\x1b[;document/lml;
~~~

The sequence is ended with the last closing parenthesis of
the following document.

~~~lml
\x1b[;document/lml;
(document
  (head
    (title "We'll render HTML anyhow!"))
  (body
    "Lisp brings elegant macro expansion "
    "to break the document down into its primitives."))
~~~

Macros need to be loaded by the renderer:

~~~lml
(macro button (id . text)
  `(box :class button
        :onclick ,id
     ,@text))
~~~

Style attributes can be provided to layout functions using
keyword/value pairs, followed by a list of layout functions
that must be compiled into the renderer.

~~~lml
(style (button)
  :padding 2
  min center)
(style (button *)
  :font 4x8-light)
~~~

~~~
\x1b[event-name;id;<data>]
~~~
