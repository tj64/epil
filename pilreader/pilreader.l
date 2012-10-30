o#{

Copyright: Axel Svensson <svenssonaxel@gmail.com>
Licence: See the COPYING file distributed with picolisp 
Version: 2012-10-30

Description:
pilreader.l implements an "alternate reader" function (r) that aims
to copy the behaviour of the built-in (read). The purpose is to
provide a start for implementing alternate picolisp readers,
especially for those wishing to change only a subset of the (read)
behaviour.

Documentation:
- (r)
  The reader function. It takes no arguments and aims to mimic the
  behaviour of (read) called without arguments.
- (r-syntax 'sym . prg)
  Defines a reader hook.
  sym - either a one-character symbol or NIL.
  prg - executed whenever the character in sym is encountered in the
        input stream, or if sym is NIL, whenever a character is
        encountered that doesn't have any r-syntax defined. prg should
        read one item from the input stream and return it.
- *r-syntax
  The global variable holding the r-syntax definitions in it's
  attribute list.

Known differences between (read) and (r):
- If an internal symbol begins with a control character and this
  character is not quoted using a backslash, (read) might discard
  it while (r) might not. If there is any reason for you to use
  control characters in source code files, you should at least
  escape them correctly.
- Reading of the anonymous symbol syntax, that is a dollar sign
  followed by a memory address, will under some circumstances be
  interpreted as an internal symbol instead. The rules of when this
  interpretation is used might differ between (read) and (r).
- (r) is based on (peek) and (char) and will therefore disregard any
  null characters in the input stream, while (read) might not always
  do so. Null characters in source code files is a bad idea anyway.
- Incorrect syntax may result in different error messages.

}#

(de r-syntax (k . body)
  (put '*r-syntax (intern k) body) )
(setq *r-syntax)

(de r ()
   (let (r "r-nested" "res" (r))
      (while (= "]" (peek)) (char))
      "res" ) )
(de "r-nested" ()
   (when (peek) (run (or (get '*r-syntax (intern (peek)))
                        (get '*r-syntax NIL) ) ) ) )

(setq "WS" (chop "# ^I^J^M"))
(de "r-skip" ()
   (while (member (peek) "WS")
      (when (= (char) "#")
         (if (= "{" (peek))
            (from "}#")
            (from "^J") ) ) ) )
(for C "WS"
  (r-syntax C ("r-skip") (r)) )

(r-syntax "(" ("r-list"))
(r-syntax "[" ("r-list"))
(de "r-list" ()
   (let ("d" () "di" () "b" (char)
      "l" (make
           (loop
              ("r-skip")
              (unless (member (peek) '(")" "]" "." "~"))
                 (when "di" (quit "Multiple forms after dot in list"))
                 (when "d" (on "di"))
                 (link (r)) )
              (when (= "." (peek))
                 (let r (r)
                    (if (= "." r)
                       (if "d" (quit "Dot twice in list") (on "d"))
                       (link r) ) ) )
              (when (= "~" (peek)) (char)
                 (when "d" (quit "Splice after dot"))
                 (chain (eval (r))) )
              (T (member (peek) '(")" "]"))) ) ) )
      (if2 (= "[" "b") (= "]" (peek))
         (char)
         (quit "Start bracket [ does not match end paren )")
         NIL
         (char) )
      (cond
         ("di" (apply cons "l"))
         ("d" (apply circ "l"))
         (T "l") ) ) )

(setq "nonsym" (chop "\"'(),[]`~{} ^I^J^M"))
(r-syntax NIL (when (member (peek) "nonsym") (quit "Bad syntax" (peek)))
   ("r-atom" (pack (make (until (member (peek) "nonsym")
                          (when (= "\\" (peek)) (char))
                          (link (char)) ) ) ) ) )
(de "r-atom" (a) (or (format a *Scl) (intern a)))

(r-syntax "\"" (pack
   (make (char)
      (use c
         (do T
            (setq c (char))
            (T (= c "\""))
            (cond
               ((= c "\\")
                (let c (char)
                   (if (= c "^J")
                      (while (member (peek) '(" " "^I")) (char))
                      (link c) ) ) )
               ((= c "\^")
                (link (char (% (char (char)) 32))) )
               (T (link c)) ) ) ) ) ) )

(r-syntax "'" (char) (cons 'quote (r)))
(r-syntax "," (char) (let a (r) (or (car (member a *Uni)) (push '*Uni a))))
(r-syntax "`" (char) (eval (r)))
(r-syntax "{"
   (prog2 (char)
      (if (make (while (<> (peek) "}")
                   (link (char)) ) )
         (pipe (prin "{" @ "}") (read))
         (box) )
      (char) ) )
#Is there a better way to get an external symbol by name?
# (extern) only works when the symbol exists, while (read)
# can read symbols that doesn't yet exist.
