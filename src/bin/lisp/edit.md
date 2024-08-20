TUNIX editor
============

The editor is invoked with function EDIT and the pathname
of the file to edit:

~~~lisp
(edit "mycode.lisp")
~~~

# Basic editing

The editor is screen-oriented, showing a window of the text
and a status line at the bottom.  The status line shows
the filename, the last type of error that occured, and the
number of bytes left which is updated sporadically.
Entered characters are inserted immediately at the current
cursor position.  Cursor keys move the cursor, backspace
and delete keys remove chars and enter opens a new line.
Removing all character from a line will delete it as well.

# Commands

Commands are started by pressing Ctrl-K, followed by one
of these characters:

| Command | Description         |
|---------|---------------------|
|    e    | Evaluate the file.  |
|    s    | Save the file.      |
|    x    | Exit the editor.    |

## Evaluating the file

When pressing Ctrl-K e, the editor will save the test as
file "etmp.img", remove most of its code from the heap and
LOAD the saved file.  By call EDIT again, the editor will be
restarted where you left off.

## Saving the file

After pressing Ctrl-K s, you will be prompted for the name
of the file, providing the known name which you can confirm
by pressing ENTER or edit before.  Errors will be shown in
the status line, should they occur.  In any case you'll
return to the editor afterwards.

## Exiting the editor

Ctrl-K x will exit the editor immediately if the text has
not been modified without saving.  Otherwise you'll be asked
to save it.  You may choose to do or not to do so, or to
continue editing, by pressing either 'y', 'n', or 'c'.
