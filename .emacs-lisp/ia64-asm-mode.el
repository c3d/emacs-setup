;;; ia64-asm.el --- mode for editing IA-64 assembler code

;; Copyright (C) 1991 Free Software Foundation, Inc.

;; Author: Christophe de Dinechin <ddd@cup.hp.com>
;; Maintainer: FSF
;; Keywords: tools, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode was written by Christophe de Dinechin <ddd@cup.hp.com>
;; inspired by earlier work from Eric S. Raymond <esr@snark.thyrsus.com>,
;; and Martin Neitzel.

;; This minor mode is based on text mode.  It defines a private abbrev table
;; that can be used to save abbrevs for assembler mnemonics.  It binds just
;; four keys:
;;
;;	TAB		tab to next tab stop
;;	:		outdent preceding label, tab to tab stop
;;	C-j, C-m	newline and tab to tab stop
;;
;; It also defines a C++ compatible comment (comments may begin either with
;; '//' for single-line comments and '/*' for multi-line comments ending in
;; '*/'

;;
;; Code is indented to the first tab stop level.

;; This mode runs two hooks:
;;   1) An asm-mode-set-comment-hook before the part of the initialization
;; depending on ia64-asm-comment-string, and
;;   2) an asm-mode-hook at the end of initialization.

;;; Code:

(defgroup ia64-asm nil
  "Mode for editing assembler code."
  :group 'languages)

(defcustom ia64-asm-comment-string "//"
  "*The comment-start string assumed by IA64-Asm mode."
  :type 'string
  :group 'asm)

(defvar ia64-asm-mode-syntax-table nil
  "Syntax table used while in IA64-Asm mode.")

(defvar ia64-asm-mode-abbrev-table nil
  "Abbrev table used while in IA64-Asm mode.")
(define-abbrev-table 'ia64-asm-mode-abbrev-table ())

(defvar ia64-asm-mode-map nil
  "Keymap for IA64-Asm mode.")

(if ia64-asm-mode-map
    nil
  (setq ia64-asm-mode-map (make-sparse-keymap))
  ;; Note that the comment character isn't set up until ia64-asm-mode is called.
  (define-key ia64-asm-mode-map ":"		'ia64-asm-colon)
  (define-key ia64-asm-mode-map "\C-c;"      'comment-region)
  (define-key ia64-asm-mode-map "\C-i"	'tab-to-tab-stop)
  (define-key ia64-asm-mode-map "\C-j"	'ia64-asm-newline)
  (define-key ia64-asm-mode-map "\C-m"	'ia64-asm-newline)
  )

(defconst ia64-asm-font-lock-keywords
  '(("\\(//.*\n\\)" 1 font-lock-comment-face)
    ("\\(/\\*.*\\*/\\)" 1 font-lock-comment-face)
    ("\\(;;\\)" 1 font-lock-constant-face)
    ("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\)?"
     (1 font-lock-function-name-face)
     (3 font-lock-keyword-face nil t))
    ("^\\s +\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-keyword-face))
  "Additional expressions to highlight in Assembler mode.")

(defvar ia64-asm-code-level-empty-comment-pattern nil)
(defvar ia64-asm-flush-left-empty-comment-pattern nil)
(defvar ia64-asm-inline-empty-comment-pattern nil)

;;;###autoload
(defun ia64-asm-mode ()
  "Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[ia64-asm-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[ia64-asm-newline]\tnewline, then tab to next tab stop.
\\[ia64-asm-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the normal comment syntax
`ia64-asm-comment-string' (which defaults to '//').

Alternatively, you may set this variable in `ia64-asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on IA64-Asm mode runs the hook `ia64-asm-mode-hook' at the end of initialization.

Special commands:
\\{ia64-asm-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "IA-64 Assembler")
  (setq major-mode 'ia64-asm-mode)
  (setq local-abbrev-table ia64-asm-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ia64-asm-font-lock-keywords))
  (make-local-variable 'ia64-asm-mode-syntax-table)
  (setq ia64-asm-mode-syntax-table (make-syntax-table))
  (set-syntax-table ia64-asm-mode-syntax-table)

  ;; Opportunity to change comment syntax here
  (run-hooks 'ia64-asm-mode-set-comment-hook)

  ;; Make our own local child of asm-mode-map
  ;; so we can define our own comment character.
  (use-local-map (nconc (make-sparse-keymap) ia64-asm-mode-map))

;;  (modify-syntax-entry	ia64-asm-comment-string
;;			"<" ia64-asm-mode-syntax-table)
  (modify-syntax-entry	?\n
			 ">" ia64-asm-mode-syntax-table)
  (let ((cs (regexp-quote ia64-asm-comment-string)))
    (make-local-variable 'comment-start)
    (setq comment-start (concat cs " "))
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip (concat cs "+[ \t]*"))
;;    (setq ia64-asm-inline-empty-comment-pattern (concat "^.+" cs "+ *$"))
;;    (setq ia64-asm-code-level-empty-comment-pattern (concat "^[\t ]+" cs cs " *$"))
;;    (setq ia64-asm-flush-left-empty-comment-pattern (concat "^" cs cs cs " *$"))
    )
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (setq fill-prefix "\t")
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list '(12 24 60))
  (run-hooks 'ia64-asm-mode-hook))

(defun ia64-asm-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]+\\(\\sw\\|\\s_\\)+$")
	(delete-horizontal-space)))
  (insert ":")
  (tab-to-tab-stop)
  )

(defun ia64-asm-newline ()
  "Insert LFD + fill-prefix, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (insert "\n")
  (if (looking-at "[ \t]*$")
      (tab-to-tab-stop))
  )

(defun ia64-asm-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun ia64-asm-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) ia64-asm-comment-string)
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1))
  )


(defun ia64-asm-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger ia64-asm-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((ia64-asm-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((ia64-asm-line-matches (format "^[^%c\n]+$" ia64-asm-comment-string))
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((ia64-asm-line-matches ia64-asm-flush-left-empty-comment-pattern)
    (insert ia64-asm-comment-string))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty. 
   ((ia64-asm-line-matches ia64-asm-code-level-empty-comment-pattern)
    (ia64-asm-pop-comment-level)
    (insert ia64-asm-comment-string ia64-asm-comment-string comment-start))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty. 
   ((ia64-asm-line-matches ia64-asm-inline-empty-comment-pattern)
    (ia64-asm-pop-comment-level)
    (tab-to-tab-stop)
    (insert ia64-asm-comment-string comment-start))

   ;; If all else fails, insert character
   (t
    (insert ia64-asm-comment-string))

   )
  (end-of-line))


(provide 'ia64-asm-mode)

;;; ia64-asm-mode.el ends here
