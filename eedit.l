# 28jul11abu
# (c) Software Lab. Alexander Burger

# "*F" "*Lst" "*X" "*K"

#{ 

* Customize Emacs:

** Make Emacs revert the edit-buffers without querying
(see http://www.gnu.org/software/emacs/manual/html_node/elisp/Reverting.html)

,--------------------------------------------------------------------
| You can customize how revert-buffer does its work by setting the
| variables described in the rest of this section. 
| 
| — User Option: revert-without-query
| 
| This variable holds a list of files that should be reverted without
| query. The value is a list of regular expressions.
`--------------------------------------------------------------------

Use a regexp like this, replacing 'my/home/directory/' with the path
to your home directory:

,-------------------------------
| my/home/directory/\.pil/tmp/.+
`-------------------------------

** Make Emacs save the edited buffer on exit without querying

Do not exit with "C-x #" (server-edit) but with "C-x C-c" or "C-u C-x
C-c":

,---------------------------------------------------------------------
| (save-buffers-kill-terminal &optional ARG)
| 
| Offer to save each buffer, then kill the current connection.
| If the current frame has no client, kill Emacs itself.
| 
| With prefix ARG, silently save all file-visiting buffers, then kill.
| 
| If emacsclient was started with a list of filenames to edit, then
| only these files will be asked to be saved.
`---------------------------------------------------------------------

}#


(de edit @
   (let *Dbg NIL
      (setq "*F" (tmp '"edit.l"))
      (catch NIL
         ("edit" (rest)) ) ) )

"blablablabla"

(de "edit" ("Lst")
   (let "N" 1
      (loop
         (out "*F"
            (setq "*Lst"
               (make
                  (for "S" "Lst"
                     ("loc" (printsp "S"))
                     ("loc" (val "S"))
                     (pretty (val "S"))
                     (prinl)
                     (for "X" (sort (getl "S"))
                        ("loc" "X")
                        (space 3)
                        (if (atom "X")
                           (println "X" T)
                           (printsp (cdr "X"))
                           (pretty (setq "X" (car "X")) -3)
                           (cond
                              ((type "X")
                                 (prin "  # ")
                                 (print @) )
                              ((>= 799999 "X" 700000)
                                 (prin "  # " (datStr "X")) ) )
                           (prinl) ) )
                     (prinl)
                     (println '(********))
                     (prinl) ) ) ) )
         ## (call 'vim
         ##    "+set isk=33-34,36-38,42-90,92,94-95,97-125"
         ##    "+map K yw:call setline(line(\"$\"), \"(\" . line(\".\") . \" \" . @@ . \")\")^MZZ"
         ##    "+map Q GC(0)^[ZZ"
         ##    (pack "+" "N")
         (call 'emacsclient
         ##    "*F" )
            "-s" "emacs1"
            "-a" ""
            "-c"
            (pack "+" "N")
            "*F")

         (apply ==== "*Lst")
         (in "*F"
            (while (and (setq "*X" (read)) (atom "*X"))
               (def "*X" (read))
               (until (= '(********) (setq "*K" (read)))
                  (def "*X" "*K" (read)) ) ) )

         (====)
         (NIL "*X" (throw))
         (T (=0 (car "*X")))
         (setq "N" (car "*X"))
         ("edit" (conc (cdr "*X") "Lst")) ) ) )

(de "loc" ("X" "Lst")
   (cond
      ((memq "X" "Lst"))
      ((and (str? "X") (not (memq "X" (made))))
         (link "X") )
      ((pair "X")
         (push '"Lst" "X")
         ("loc" (car "X") "Lst")
         ("loc" (cdr "X") "Lst") ) ) )
