# close all open parenthesis (from Joe Bogner)
(de closeParens (Line)
    (let (Open (length (sect Line (list "(" ))) 
                Close (length (sect Line (list ")" ))) )
        (make 
            (for X Line (link X))
            (for X (- Open Close) (link ")")) ) ) )

# keybindings
(setq *EmacsKeyHook
   '(("^v" (and (key 500)   # ^v as prefix for hook functions
              (case @
                 ("^c"
                    (EmacsHook
                       (let L (closeParens _Line)
                          (_chgLine L (length L)) ) ) ) ) ) ) ) )
