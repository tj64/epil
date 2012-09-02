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

1. Copy `elib.l` to `/picolisp/`
2. Copy `epil` to `/picolisp/bin/` and make it executable (!)
3. Copy `eedit.l` and the other `e` prefixed libraries to
`/picolisp/lib/`
4. create a symlink: `# ln -s /usr/lib/picolisp/bin/epil /usr/bin/epil`

Instead of copying the files, it might be preferable to clone this
gitrepo and *symlink* the files from the repo to the respective
directories in the global PicoLisp installation (as root).

## Disclaimer ##

This is work in progress and *not yet functional*. Once it works, it
will be released as version 1.0 and anounced as such. 
