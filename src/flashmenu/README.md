# Ultimem boot menu and file manager

## Overview

This is the program that'll be started by src/flashboot.
It's supposed to become a "Norton Commander"–style file
manager to move and copy files across floppy/SD2IEC,
tape and the Ultimem's Flash ROM.

## The Graphical User Interface

Everything displayed are elements connected via a tree of lists,
very much like a HTML document. Some elements aren't visible but
layout the elements they contain. All elements can also be
assigned a layout function that modifiy their position and size.

Since this is work under construction the only source for
detailed information is the source itself.

### Visible elements

* button (framed text)
* scrollable (displaying a part of another screen)
* window

### Structural elements

* frame (keeping elements in one area to support layout functions)
* list (horizontal or vertical)
* table
