(provide 'findgrep)

;;;
;;;
;;;  Extended search tool
;;;
;;;

(defvar find-grep-history nil)
(defun find-grep (command-args)
  "Run find/grep"
  (require 'compile "compile")
  (interactive
   (list (adaptative-read "Find and grep: "
                          find-grep-command nil nil 'find-grep-history)))
  (compile-internal (concat command-args " /dev/null; sleep 5")
		    "No more hits" "find-grep"
		    ;; Give it a simpler regexp to match.
		    nil grep-regexp-alist))

(defun previous-error (n)
  "Find previous error in compilation buffer"
  (interactive "p")
  (next-error (- n)))

;;;
;;; Avoid reading the target and options everytime for text searchs.
;;;
(defvar manygrep-target "*.C *.h *.c *.y *.H *.cpp *.m *.xl *.xs *.ddd *.hh *.cc *.tbl *.html *.js *.ejs *.rb *.erb *.txt"
  "Target(s) for manygrep")

(defvar manygrep-options "i"
  "Options for manygrep")

(defvar manygrep-topdir "."
  "Top directory for manygrep recursive searches")

(defvar manygrep-topdir-hist '(".")
  "History of manygrep top directories")

(defvar manygrep-target-hist nil
  "History of manygrep regexps")

(defun store-manygrep-values ()
  (interactive)
  (setq manygrep-target (read-string "ManyGrep target: " manygrep-target))
  (setq manygrep-options (read-string "ManyGrep options: " manygrep-options))
  (setq manygrep-topdir
        (adaptative-read "Top directory (for recursive searches): "
                         manygrep-topdir nil nil 'manygrep-topdir-hist))
)

(defun manygrep (st)
  "Run grep with pre-stored target(s) and options"
  (interactive
   (list (adaptative-read "[grep]Regexp: " "" nil nil
                          'manygrep-target-hist)))

  (let ((grep-command
         (concat "grep -n" manygrep-options " -- "  "\"" st "\" "
                 manygrep-target)))
    (grep grep-command))
)

(defun manygrep-git (st)
  "Run git-grep with pre-stored target(s) and options"
  (interactive
   (list (adaptative-read "[git-grep]Regexp: " "" nil nil
                          'manygrep-target-hist)))

  (let ((grep-command
         (concat "GIT_PAGER=cat git grep -n" manygrep-options " -- " 
                 "\"" st "\""))
        (grep-use-null-device nil))
    (grep grep-command))
)

(defun manyfindgrep (st)
  "Run find&grep with pre-stored target(s) and options (from stored directory)"
  (interactive
   (list (adaptative-read "[findgrep]Regexp: " "" nil nil
                          'manygrep-target-hist)))

  (let ((grep-command
         (concat "find '" manygrep-topdir "' \\( -type f "
                 (findify-names manygrep-target) " \\) -print0 "
                 "| xargs -0 -n30 grep -n" manygrep-options " -- \"" st "\""
                 " /dev/null; sleep 5 < ")))
    (grep grep-command))
)

(defun findify-names (string)
  "Transform a list of file names to make it suitable for the find command"

  (let ((s string)
        (r ""))
    ;; Remove leading and trailing space(s)
    (and (string-match "^[ \t]+" s)
         (setq s (substring s (match-end 0))))
    (and (string-match "[ \t]+$" s)
         (setq s (substring s 0 (match-beginning 0))))

    ;; Insert ``find'' conjunctions
    (while (string-match "[ \t]+" s)
      (setq r (concat r "-name \""
                      (substring s 0 (match-beginning 0)) "\" -o "))
      (setq s (substring s (match-end 0))))
    (setq r (concat r "-name \"" s "\"")))
)

(defun adaptative-read (a b c d e)
  "
Call read-from-minibuffer with the appropriate number of args according to
Emacs version.
"
  (read-from-minibuffer a b c d e))
