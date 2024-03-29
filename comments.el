(provide 'comments)

;;;
;;;
;;;  Comment insertion and other text utilities
;;;
;;;

(defvar comment-width 80)

(defun insert-repeated (srepeat sfill beginning repeated ending)
  "Insert a repeated pattern until end of line is met"
  (insert beginning)
  (while (< (+ (current-column)
               (length repeated)
               (length ending))
            srepeat)
    (insert repeated))
  (while (< (+ (current-column)
               1
               (length ending))
            sfill)
    (insert " "))
  (insert ending))

(defun insert-nl ()
  (interactive)
  (insert "
"))

;;; Quick comment insertion
(defun insert-comment (comment-text)
  (if (string= comment-end "")
      (insert comment-start comment-text)

    (insert-repeated comment-width comment-width
                     (concat comment-start
                             comment-text)
                     " " comment-end))
  (insert-nl))

(defun insert-comment-line (comment-char)
  (if (and (string= comment-end "") (string= comment-char " "))
      (insert comment-start)
    (insert-repeated comment-width comment-width
                     comment-start comment-char comment-end))
  (insert-nl))

(defun insert-multiline-comment (comment-text)
  (if (= (length comment-end) 0)
      (progn
       (insert comment-start)
       (insert comment-text)
       (insert-nl))
    (insert-repeated (1+ (length comment-start)) 0 "" " " "")
    (insert comment-text)
    (insert-nl)))

;; Inserting revision info, works for multiline comments or when
;; comment-end is empty
(defun insert-revision-info ()
  (insert-repeated comment-width comment-width comment-start "*" "")(insert-nl)
  (insert-multiline-comment "* File       : $RCSFile$")
  (insert-multiline-comment "* Revision   : $Revision$")
  (insert-multiline-comment "* Date       : $Date$")
;  (insert-multiline-comment "* $Log$")
  (if (= (length comment-end) 0)
      (insert-comment-line "*")
    (insert-repeated (1+ (length comment-start)) 0 "" " " "")
    (insert-repeated comment-width comment-width "" "*" comment-end)))

(defcustom ddd-project-name "DB48X"
  "Name of the project for header commands")

(defun insert-xlr-header ()
  "Insert standard header at point"
  (interactive)
  (insert-comment-line "*")
  (insert-repeated comment-width comment-width
                   (concat comment-start " " (buffer-name))
                   " "
                   (concat ddd-project-name " project" comment-end))
  (insert-nl)
  (insert-comment-line "*")
  (insert-comment-line " ")
  (insert-comment "  File Description:")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line " ")
  (insert-comment-line "*")
  (insert-comment
   (format "  (C) %s Christophe de Dinechin <christophe@dinechin.org>"
           (calendar-extract-year (calendar-current-date))))
  (insert-comment "  This software is licensed under the terms outlined in LICENSE.txt")
  (insert-comment-line "*")
  (insert-gpl-header)
  (insert-comment-line "*"))

(defun insert-gpl-header ()
  "Insert a standard GPL header"
  (interactive)
  (insert-comment (concat "  This file is part of " ddd-project-name "."))
  (insert-comment "")
  (insert-comment (concat "  " ddd-project-name
                          " is free software: you can redistribute it and/or modify"))
  (insert-comment "  it under the terms outlined in the LICENSE.txt file")
  (insert-comment "")
  (insert-comment (concat "  " ddd-project-name " is distributed in the hope that it will be useful,"))
  (insert-comment "  but WITHOUT ANY WARRANTY; without even the implied warranty of")
  (insert-comment "  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."))


;; Commenting current region line by line
(defun comment-region-large (beg end &optional arg)
  (interactive "*r\nP")
  (save-excursion
    (save-restriction
      (if arg (setq comment-width (prefix-numeric-value arg)))
      (narrow-to-region beg end)
      (if (<= comment-width 0)
          (progn
            (goto-char beg)
            (setq comment-width 0)
            (while (not (eobp))
              (setq line-beg (point))
              (end-of-line)
              (setq line-width (- (point) line-beg))
              (if (> line-width comment-width) (setq comment-width line-width))
              (forward-char))
            (setq comment-width (+ comment-width
                                   1
                                   (length comment-start)
                                   (length comment-end)))))
      (goto-char beg)
      (while (not (eobp))
        (insert comment-start)
        (end-of-line)
        (insert-repeated comment-width comment-width "" " " comment-end)
        (end-of-line)
        (forward-char)))))


;; Special types of comments
(defun insert-dash-comment ()
  "Insert comment line of '-' signs"
  (interactive)
  (insert-comment-line "-"))
(defun insert-equal-comment ()
  "Insert comment line of '=' signs"
  (interactive)
  (insert-comment-line "="))
(defun insert-star-comment ()
  "Insert comment line of '*' signs"
  (interactive)
  (insert-comment-line "*"))
(defun insert-space-comment ()
  "Insert comment with line of spaces"
  (interactive)
  (insert-comment-line " "))
(defun insert-percent-comment ()
  "Insert comment line of percent signs"
  (interactive)
  (insert-comment-line "%"))

(defun unhook-trailing-whitespace ()
  "Remove hook removing trailing whitespace from current buffer"
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

(defun LISO-comment (beg end level)
  "
Comment a region according to LISO comment levels :
  1) ................., (Use \"*\" for header line)
  2) Section separator, (Use \"=\" for header line)
  3) Procedure/function description, (Use \"-\" for header line)
"

  (interactive "*r\nsComment level: ")
  (setq level (cond ((member level '("*" "F" "f")) 1)
                    ((member level '("=" "S" "s")) 2)
                    ((member level '("-" "P" "p" "")) 3)
                    (t (string-to-number level))))

  (let (c movbeg movend)
    (and (< level 0) (setq level 0))
    (and (> level 3) (setq level 3))
    (setq c (cond ((= level 1) "*")
                  ((= level 2) "=")
                  ((= level 3) "-")))

    (save-excursion
      (save-restriction
        (goto-char end)
        (setq movend (point-marker))
        (goto-char beg)
        (setq movbeg (point-marker))
        (comment-region-large beg end)
        (and (member level '(1 2))
             (goto-char movbeg) (insert-comment-line " "))
        (goto-char movbeg)
        (insert-comment-line c)
        (goto-char movend)
        (insert-comment-line c)
        (and (member level '(1 2))
             (goto-char movend) (insert-comment-line " "))
        )
      )
    )
)


(defun use-cplusplus-style-comments ()
  "Use // as comment separator"
  (interactive)
  (set-variable (quote comment-start) "// " nil))
