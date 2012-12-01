PicoLisp Line Editing 
====

# Emacs-like editing for PicoLisp #

[PicoLisp](file:///home/tj/bin/picoLisp/doc/tut.html) permanently
reads input from the current input channel (i.e. the console in
interactive mode), evaluates it, and prints the result to the current
output channel. This is called a "read-eval-print-loop" (REPL).

## 'vi'-style  ##

This is the default line editor, as it needs less system resources and
works also on dumb terminals. It is similar to -- though simpler than
-- the 'vi' edit modes of the 'korn' and 'bash' shells. For an analog
'emacs' style editor, please see below.

## 'emacs'-style ##

You can switch the command line editor to an 'emacs' style, if you
call the function `(em)` (i.e. without arguments). A single call is
enough. Alternatively, you could invoke PicoLisp at least once with
the `-em` command line option


    $ pil -em +
    :

The style will be remembered in a file "~/.pil/editor", and used in
all subsequent PicoLisp sessions.

To switch back to 'vi' style, call `(vi)`, use the `-vi` command line
option, or simply remove "~/.pil/editor".


# The Key-Bindings Reference #

It is very helpful - though not absolutely necessary - when you know
how to use the Emacs editor.

To alleviate the task of manual line input for people not used to
*vi*, a (second) command line editor is provided which is modeled
after Emacs. It is loaded at startup in debug mode (see above), you
find its source in "lib/eled.l".

For Emacs, in contrast to Vi, there is only the Insert Mode. You can
enter lines in the normal way, correcting mistypes with the BACKSPACE
key, and terminating them with the ENTER key.

The key commands in the Emacs-mode of the PicoLisp command line are
listed below. 

    
    * Tabular overview of keybindings used by *eled*
    
    ** Starting PicoLisp with Emacs-style line-editing
    
    | action        | comment                           |
    |---------------+-----------------------------------|
    | $ ./pil -em + | 'pil +' with "-em" shell argument |
    |---------------+-----------------------------------|
    | $ ./pil +     | or simply 'pil +'                 |
    | : (em)        | and then '(em)' in the REPL       |
    
    
    ** Files and Directories 
    
    | action                                | keys        |
    |---------------------------------------+-------------|
    | show pwd                              | C-M-d       |
    | change directory                      | C-c C-d     |
    | make directory                        | C-c +       |
    | list directory files                  | C-x C-d     |
    | list directory files (with dot-files) | C-u C-x C-d |
    | dired-like file list                  | C-x d       |
    | dired-like file list (with dot-files) | C-u C-x d   |
    | find file (emacsclient)               | C-x C-f     |
    | find file (zile)                      | C-x f       |
    | line number(s) of file(s)             | C-x l       |
    
    
    
    
    ** Help/Info/Debugging
    
    | action                | keys      |
    |-----------------------+-----------|
    | debug                 | C-h d     |
    | unbug                 | C-u C-h d |
    | file info             | C-h i     |
    | symbol doc            | C-h f     |
    | show symbol           | C-h s     |
    | pretty print (pp)     | C-h p p   |
    | pretty print (pretty) | C-h p r   |
    |                       |           |
    
    
    ** Error Recovery
    
    | action        | keys         |
    |---------------+--------------|
    | abort command | C-g          |
    | undo          | C-x u or C-_ |
    | quit          | C-q          |
    
    
    ** (Incremental) Search
    
    | action           | keys |
    |------------------+------|
    | goto (find) char | M-g  |
    
    
    ** Motion
    
    | entitiy to move over | backward | forward |
    |----------------------+----------+---------|
    | character            | C-b      | C-f     |
    | word                 | M-b      | M-f     |
    | line up/down         | C-p      | C-n     |
    | line beg/end         | C-a      | C-e     |
    | sexp                 | C-M-b    | C-M-f   |
    
    The arrow keys can be used for navigation too. 
    
    ** Killing and Deleting
    
    | entitiy          | backward    | forward |
    |------------------+-------------+---------|
    | character        | BACKSPACE   | C-d     |
    | word             | M-BACKSPACE | M-d     |
    | line (to end of) |             | C-k     |
    
    
    | action            | keys  |
    |-------------------+-------|
    | kill sexp         | C-M-k |
    | yank              | C-y   |
    | content kill-ring | C-h r |
    
    
    ** Case Change
    
    | action           | keys       |
    |------------------+------------|
    | toggle char case | M-c or M-l |
    
    
    ** The Command Line 
    
    | action                       | keys  |
    |------------------------------+-------|
    | complete word                | TAB   |
    | execute                      | RET   |
    | history search pattern input | C-M-s |
    | history search forward       | M-s   |
    | history search backward      | M-r   |
    
    
    ** Tags
    
    | action       | keys |
    |--------------+------|
    | find tag     | M-.  |
    | pop tag-mark | M-*  |
    
    works in Emacs after using (em 'fun) for editing PicoLisp sources
    
    ** Shell
    
    | action                      | keys         |
    |-----------------------------+--------------|
    | call shell-command          | C-c C-c      |
    |                             | (or C-c C-!) |
    | clear screen                | C-l          |
    | terminate process           | C-q          |
    | interrupt program execution | C-r          |


# Custom Key Bindings

Custom key bindings can be added through a hook that is executed whenever a key is pressed.

There are two symbols used when creating a hook.

*EmacsKeyHook is a global variable that holds the list of keys and the fexpr to be invoked when the key is hit.

EmacsHook is a function defined in eled.l that wraps the transient symbols of the current Line "Line" into _Line and the chgLine function into _chgLine. The preceding underscore is used to prevent collisions with the transient. 

For example,  

If you have a file called init.l with the following

	(de closeParens (Line)
		(let (Open (length (sect Line (list "(" ))) 
					Close (length (sect Line (list ")" ))))
			(make 
				(for X Line (link X))
				(for X (- Open Close) (link ")")))))

	(setq *EmacsKeyHook '(("^o" (EmacsHook (let L (closeParens _Line) (_chgLine L (length L)))))))


And if you invoke pil with
	./pil init.l -em +

You will then have a key binding that automatically closes open parentheses when c-o is hit.


### Notes: ###

  * Search patterns may contain "@" characters as wildcards.
  * Lines shorter than 3 characters, lines beginning with a space character, or
    duplicate lines are not entered into the history.
  * The history is stored in the file ".pil/history" in the user's home
    directory. The length of the history is limited to 1000 lines.

The following two key-combinations are special:

  * Ctrl-Q (C-q or ^q) will immediately terminate the current process. 
  * Ctrl-G (C-g or ^g) discards all input, abandons further
    processing, and returns to the interpreter's top level (equivalent
    to invoking quit). This is also useful when the program stopped at
    a breakpoint (see single-stepping Debugging), or after program
    execution was interrupted with Ctrl-R.

Besides these two keys, in Insert Mode only the following keys have a special
meaning:

  * BACKSPACE and DEL erase the character to the left
  * TAB performs symbol and/or path completion: When a symbol (or path) name is
    entered partially and TAB is pressed subsequently, all internal symbols
    (and/or path names in the file system) matching the partial input are shown
    in sequence. 
  * ESC-key sequence can be used instead of M-key (ALT-key).

Conclusion

Please take some time to experiment and to get used to command line editing. It
will make life much easier in the future :-)


## Disclaimer ##

This is work in progress and *still in early state*. 
