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

# Objects

Using SLOT-VALUE and abbreviating dot notation on
associative lists.  ASSOC should be a built-in function to
avoid performance issues.

?: How about immutables?

# Directory access

| Function      | Description                   |
|---------------|-------------------------------|
| (mkdir s)     | Create directory.             |
| (opendir n s) | Open directory on channel.    |
| (readdir n)   | Read directory info.          |
| (writedir n)  | Write partial directory info. |

## (mkdir s): Create directory.
## (opendir n s): Open directory on channel.
## (readdir n): READ directory info.
## (writedir n): Write partial directory info.

# Exceptions

Catch stack.

# Real-time applications

Interruptible GC with lower threshold to keep space for
critical operations is a bad idea as a GC has to complete at
some point.

| Function  | Description                  |
|-----------|------------------------------|
| (gc x)    | GC with another root object. |

GC could take an optional argument to specify another root
than variable \*universe\* to discard everything that is
not part of an app.

~~~lisp
(gc 'appstart)
; Put APPSTART back in the universe.
(var appstart appstart)
~~~

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

# Compressed lists

Conses which only store the CAR if the CDR is the next
object on the heap.  This can be done at allocation time but
would make the list's CDRs immutable.

The disadvantage is that extra checks are required to access
a CDR.

# Processes

Sharing the heap they forked off from.  With pipelining.

# Wanted

* Math lib for lists of decimals of arbitrary length.
