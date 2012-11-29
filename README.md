epil
====

# PicoLisp Line Editing #

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


### Emacs-like editing for PicoLisp ###

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
  * C-x u (^x u) or C-_ (^_) - Undo the last change (one level only)
  * [U - Undo all changes of the current line] **TODO**
  * C-y (^y) - Display current contents of cut buffer **[???]**

Notes:

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

This is work in progress and *still in alpha state*. 
