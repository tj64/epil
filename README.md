epil
====

Emacs-style **PicoLisp** command line - non-modal line editing with
Emacs keybindings.

## Global Installation ##

This assumes that you finished successfully the global *PicoLisp*
installation as described in the *PicoLisp*
[INSTALL](http://software-lab.de/INSTALL) file. 

To enable `eled.l` and `eedit.l`, the Emacs variants of `led.l` and
`edit.l`, in a global PicoLisp installation, do the following (as
root, changing owner and group of each file to `root`): 

1. Copy `elib.l`, `edbg` and `edbg.l`  to `/picolisp/` (make `edbg` executable)
2. Copy `epil` to `/picolisp/bin/` and make it executable 
3. Copy `eedit.l` and the other `e` prefixed libraries to
`/picolisp/lib/`
4. create a symlink: `# ln -s /usr/lib/picolisp/bin/epil /usr/bin/epil`

Instead of copying the files, it might be preferable to clone this
gitrepo and *symlink* the files from the repo to the respective
directories in the global PicoLisp installation (as root).

## Invocation ##

In a global installation, the 'epil' command should be used. You can either start
in plain or in debug mode. The difference is that for debug mode the command is
followed by single plus ('+') sign. The '+' must be the very last argument on
the command line.

    $ epil       # Plain mode
    :

    $ epil +     # Debug mode
    :

In both cases, the colon ':' is PicoLisp's prompt. You may enter some Lisp
expression,

    : (+ 1 2 3)
    -> 6

To exit the interpreter, enter

    : (bye)

or just type Ctrl-Q.


For a local invocation, specify a path name, e.g.
    
    $ ./epil     # Plain mode
    :

    $ ./epil +   # Debug mode
    :

or

    $ /home/app/epil  # Invoking a local installation from some other directory


A shortcut for debug mode is the 'edbg' script:

    $ ./edbg
    :

It is available only for local installaions, and is eqivalent to

    $ ./epil +

Note that 'epil' can also serve as a template for your own stand-alone scripts.


## Emacs-like editing for PicoLisp ##

It is very helpful - though not absolutely necessary - when you know
how to use the Emacs editor.

To alleviate the task of manual line input, a command line editor is
provided which is similar to (though much simpler than) the readline
feature of the bash shell. Only a subset of the Emacs mode is
supported. It is loaded at startup in debug mode, you find its source
in "lib/eled.l".

For Emacs, in contrast to Vi, there is only the Insert Mode. You can
enter lines in the normal way, correcting mistypes with the BACKSPACE
key, and terminating them with the ENTER key.

The key commands in the Emacs-mode of the PicoLisp command line are
listed below. Deleting or changing a "word" take either the current
atom (number or symbol), or a whole expression when the cursor is at a
left parenthesis.

  * C-p (^p) - Go up one line
  * C-n (^n) - Go down one line
  * C-f (^f) - Go right one character
  * C-b (^b) - Go left one character
  * M-f (ESC-f) - Go right one word
  * M-b (ESC-b) - Go back (left) one word
  * C-a (^a) - Go to the beginning of the line
  * C-e (^e) - Go to the end of the line
  * C-d (^d) - Delete the character at the cursor position
  * BACKSPACE - Delete the character left of the cursor position
  * M-d (ESC-d) - Delete the word at the cursor position 
  * C-k (^k) - Delete the rest of the line
  * [f - Find next key in the rest of the current line] **TODO**
  * C-y (^y) - Paste data deleted with x, X, d or D before the cursor position
  * C-s (^s) or C-r (^r) - Accept an input pattern and search the history for it
  * [n - Search for next occurrence of pattern (as entered with /)] **TODO**
  * [N - Search for previous occurrence of pattern] **TODO**
  * [C-M-f (ESC^s) or C-M-b (ESC^r) - Go to matching parenthesis] **TODO**
  * M-c (ESC-c) or M-l (ESC-l) -
     Convert character to opposite (lower or upper) case and move right
  * u - Undo the last change (one level only)
  * [U - Undo all changes of the current line] **TODO**
  * C-y (^y) - Display current contents of cut buffer **[???]**

Notes:

  * Search patterns may contain "@" characters as wildcards.
  * Lines shorter than 3 characters, lines beginning with a space character, or
    duplicate lines are not entered into the history.
  * The history is stored in the file ".pil/history" in the user's home
    directory. The length of the history is limited to 1000 lines.

The following two key-combinations work both in Insert and Command Mode:

  * Ctrl-Q (C-q or ^q) will immediately terminate the current process.
  * Ctrl-G (C-g or ^g) discards all input, abandons further
    processing, and returns to the interpreter's top level (equivalent
    to invoking quit). This is also useful when the program stopped at
    a breakpoint (see single-stepping Debugging), or after program
    execution was interrupted with Ctrl-C.

Besides these two keys, in Insert Mode only the following keys have a special
meaning:

  * BACKSPACE (Ctrl-H **[??]**) and DEL erase the character to the left
  * TAB performs symbol and/or path completion: When a symbol (or path) name is
    entered partially and TAB is pressed subsequently, all internal symbols
    (and/or path names in the file system) matching the partial input are shown
    in sequence. **TODO**
  * ESC-key sequence can be used instead of M-key (ALT-key).

Conclusion

Please take some time to experiment and to get used to command line editing. It
will make life much easier in the future :-)


## Disclaimer ##

This is work in progress and *still in experimental state*. 
