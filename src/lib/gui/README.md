# Ingle's GUI library

Provides an object-oriented document tree with windows on top.

## Objects

Objects a connected to a tree of doubly-linked lists.  Each
object provides entries for drawing, layouting, freeing
additional resources and a pointer to an event handler.

* obj – The tree node with common interface.
** box – Filled box
** button – Button with text
** frame – Box of lines
** list – List of objects
*** table – Table of objects
** window – Window with title and content
