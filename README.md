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

In a global installation, the 'pil' command should be used. You can either start
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

or just type Ctrl-D.


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

## Disclaimer ##

This is work in progress and *not yet functional*. Once it works, it
will be released as version 1.0 and anounced as such. 
