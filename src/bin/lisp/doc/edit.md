TUNIX editor
============

The editor is invoked with function EDIT.  If no filename
has been passed as the argument, and no file is in \*LINES\*
already, the help file "edit-help.md" is loaded.

~~~lisp
; Start with help file.
(edit)

; Load file "mycode.lisp" before start.
(edit "mycode.lisp")
; (Exit with Ctrl-K, Q.)

; Continue with "mycode.lisp".
(edit)
~~~

# Basic editing

The editor is screen-oriented, showing a window of the text
and a status line at the bottom.  The status line shows
the filename, the last type of error that occured, and the
number of bytes left which is updated sporadically.
Entered characters are inserted immediately at the current
cursor position.  Cursor keys move the cursor, backspace
and delete keys remove chars and enter opens a new line.
Removing all characters from a line will delete it as well.

CTRL key combinations allow more navigation:

| Command     | Description            |
|-------------|------------------------|
| Ctrl+A      | Move to start of line. |
| Ctrl+E      | Move to end of line.   |
| Ctrl+L      | Redraw screen.         |
| Ctrl+K, <?> | Start command...       |

# Commands

Commands are started by pressing Ctrl-K, followed by one
of these characters:

| Command | Description                       |
|---------|-----------------------------------|
|    e    | Input and evaluate an expression. |
|    r    | Save and run file.                |
|    s    | Save the file.                    |
|    q    | Exit the editor.                  |

## Evaluating the file

When pressing Ctrl-K r, the editor will save the file by
the name you provide and LOAD it.  You're asked to press
ENTER when finished.
If you had to leave the editor, e.g. by pressing 'q' in the
debugger, you can continue editing by just calling EDIT
again.

## Saving the file

After pressing Ctrl-K s, you will be prompted for the name
of the file, providing the known name which you can confirm
by pressing ENTER or edit before.  Errors will be shown in
the status line, should they occur.  In any case you'll
return to the editor afterwards.

## Exiting the editor

Ctrl-K x will exit the editor immediately if the text has
not been modified.  Otherwise you'll be asked to save it.
You may choose to do or not to do so, or to continue editing,
by pressing either 'y', 'n', or 'c'.

# Wishlist

## Keyword highlighting and ompression by splitting up line symbols

At punctuation, spaces, and the rest.  Highlighting colours
can be determined using predicates like BUILTIN? and MACRO?, etc.

## Renaming symbols only for display

To keep memory consumption low.
