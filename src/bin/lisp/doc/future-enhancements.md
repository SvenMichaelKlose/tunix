---
title: "TUNIX Lisp"
subtitle: "Future Enhancements"
author: "Sven Michael Klose"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
toc: true
footnodes-pretty: true
...

# Overview

A couple of ideas to enhance TUNIX Lisp significantly.

# User-defined setters

~~~lisp
(setcar x v)
(= (car x) v)
~~~

# Multi-purpose operators

## '+' to also append lists

~~~lisp
; Same:
(append a b)
(+ a b)
~~~

# Exceptions

setjmp() buffers on tag stack.

# Real-time applications

Copying GC is the only way to go in my mind.  Goes well
with banked memory.
Interruptible GC with lower threshold to keep space for
critical operations is a bad idea as a GC has to complete at
some point.

# Bielefeld DB

| Function     | Description                     |
|--------------|---------------------------------|
| (db-open a)  | Open database.                  |
| (db-add s x) | Add expression with string key. |
| (db-find s)  | Find ID by key.                 |
| (db-read n)  | READ by ID.                     |
| (db-close n) | Close database.                 |

## (db-open a): Open database.
## (db-add s x): Add expression with string key.
## (db-find s): Find ID by key.
## (db-read n): READ by ID.
## (db-close n): Close database.

Embedded database to the rescue the day for large data sets.

# Defining built-ins

| Function        | Description            |
|-----------------|------------------------|
| (mkbuiltin s a) | Add built-in function. |

Submit to your fantasy.

# Processes

Sharing the heap they forked off from.  With pipelining.

# Wanted

* Math lib for lists of decimals of arbitrary length.
