;; * FIXME: 
;; ** recognition of unit via point not region
;; ** bind C-u C-x C-c to one key (F??)


(defun pil-edit-K ()
  "Write symbol at point with line number in last line of edit-buffer.

If the symbol is a transient symbol, write it with double-quotes,
otherwise as unquoted word. The output-format is: 

\(<line-number> <symbol>\)
 e.g.
\(50  edit\)
\(56 \"edit\"\)

when point is on the edit or \(transient\) \"edit\" symbol in the
PicoLisp sourcefile edit.l and `pil-edit-K' is called (the
line-numbers may be different in your version of edit.l).

Recognition of transient symbols works by getting the
text-property 'face' at point and checking if it is equal to
'font-lock-string-face'. Thus, this function works correctly only
if the edit-buffer is in an Emacs major-mode that fontifies
strings with 'font-lock-string-face' \(like `picolisp-mode'
does\)."

  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (unless (mark 'FORCE)
        (forward-word)
        (forward-word -1)
        (mark-word))
      (let* ((thing (thing-at-point 'word))  ; FIXME better use 'symbol ?
             (unit (get-selection-or-unit 'word))
             (line (line-number-at-pos))
             (transient-p
              (string-equal (get-text-property (point) 'face)
                            "font-lock-string-face"))
             (k-list nil))
        (setq k-list (list line
                           (if transient-p
                               (elt unit 0)
                             (make-symbol (elt unit 0)))))
        (message "K-list: %S transient: %S" k-list transient-p)
        (goto-char (max-char))
        (newline)
        (insert (format "%S" k-list))
        (save-buffers-kill-terminal 1)))))


(defun pil-edit-Q ()
  "Write '(0)' in last line of PicoLisp edit-buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (max-char))
      (newline)
      (insert "(0)")
      (save-buffers-kill-terminal 1))))
      

;; from: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html 
;; Xah Lee, 2011-03-16, …, 2012-07-17
;; This page explains the elisp functions get-selection-or-unit ＆
;; unit-at-cursor, contained in the package 〔xeu_elisp_util.el〕.
;; Get the latest version at:
;; http://code.google.com/p/ergoemacs/source/browse/packages/xeu_elisp_util.el
;; For reasons why it's written, see: Emacs Lisp: Using thing-at-point.
;; get-selection-or-unit

(defun get-selection-or-unit (unit)
  "Return the string and boundary of text selection or UNIT under cursor.

If `region-active-p' is true, then the region is the unit.  Else,
it depends on the UNIT. See `unit-at-cursor' for detail about
UNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (get-selection-or-unit 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)

  (let ((p1 (region-beginning)) (p2 (region-end)))
    (if (region-active-p)
        (vector (buffer-substring-no-properties p1 p2) p1 p2 )
      (unit-at-cursor unit) ) ) )

;; This function get-selection-or-unit gets you the text selection if
;; there's one. If not, it calls unit-at-cursor. unit-at-cursor

(defun unit-at-cursor  (unit)
  "Return the string and boundary of UNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

UNIT can be:
• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.
• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, that doesn't have spaces in it.
• 'line — delimited by “\\n”.
• 'block — delimited by “\\n\\n” or beginning/end of buffer.
• 'buffer — whole buffer. (respects `narrow-to-region')
• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
    (setq bds (unit-at-cursor 'line))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:
• this function returns the text and the 2 boundaries as a vector in one shot.
• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.
• 'word does not depend on syntax table.
• 'block does not depend on syntax table."
  (let (p1 p2)
    (save-excursion
      (cond
       ( (eq unit 'word)
         (let ((wordcharset "-A-Za-zÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point)))
         )

       ( (eq unit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point)))
         )

       ( (eq unit 'buffer)
         (progn
           (setq p1 (point-min))
           (setq p2 (point-max))
           )
         )

       ((eq unit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq unit 'block)
        (progn
          (if (re-search-backward "\n\n" nil t)
              (progn (forward-char 2)
                     (setq p1 (point) ) )
            (setq p1 (line-beginning-position) )
            )

          (if (re-search-forward "\n\n" nil t)
              (progn (backward-char)
                     (setq p2 (point) ))
            (setq p2 (line-end-position) ) ) ))

       ((vectorp unit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt unit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt unit 1))
          (setq p2 (point))))
       ) )

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )
    ) )

;; This function unit-at-cursor is similar to thing-at-point and
;; bounds-of-thing-at-point.

;; Here are the main differences.

;;     This function returns the text and the boundaries as a vector in
;;     one shot. It saves you 3 lines of manually extracting the string
;;     when you also need the boundary. 'line always returns the line
;;     without end of line character, avoiding inconsistency when the
;;     line is at end of buffer. 'word does not depend on syntax table.
;;     'block does not depend on syntax table of “paragraph”. It's always
;;     delimited by 2 blank lines or beginning/end of file. 'glyphs is a
;;     sequence of visible glyphs. Useful for getting file path, URL, ….
;;     A user defined boundary can be specified, by a vector [beginRegex
;;     endRegex]. It's passed to skip-chars-backward and
;;     skip-chars-forward. For example, if you want paren as delimiter,
;;     use ["^(" "^)"].

;;     All About Processing Lines in Emacs Lisp Emacs Lisp's print,
;;     princ, prin1, format, message Emacs Lisp: Multi-Pair String
;;     Replacement: xfrp_find_replace_pairs.el

;; ∑ ErgoEmacs Home © 2006, …, 2012 Xah Lee.
  
