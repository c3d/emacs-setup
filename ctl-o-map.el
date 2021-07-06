(provide 'ctl-o-map)

;;; ------------------------------------------------------------------------
;;;
;;; Control-O map
;;;
;;; ------------------------------------------------------------------------

(defvar ctl-o-map (make-sparse-keymap)
  "Keymap for fast access to programmers commands.")
(defalias 'ctl-o-prefix ctl-o-map)

(define-key global-map "\C-o" 'ctl-o-prefix)

(define-key ctl-o-map "b" 'gdb)
(define-key ctl-o-map "c" 'compile)
(define-key ctl-o-map "d" 'c-indent-define)
(define-key ctl-o-map "f" 'find-grep)
(define-key ctl-o-map "g" 'grep)
(define-key ctl-o-map "h" 'insert-LISO-header)
(define-key ctl-o-map "i" 'overwrite-mode)
(define-key ctl-o-map "l" 'goto-line)
(define-key ctl-o-map "m" 'man)
(define-key ctl-o-map "n" 'gnus)
(define-key ctl-o-map "p" 'comment-region-large)
(define-key ctl-o-map "r" 'delete-trailing-whitespace)
(define-key ctl-o-map "s" 'shell)
(define-key ctl-o-map "t" 'talk-connect)
(define-key ctl-o-map "y" 'rotate-yank-pointer)
(define-key ctl-o-map "\\" 'c-backslash-region)

(define-key ctl-o-map "\C-f" 'fill-region)
(define-key ctl-o-map "\C-n" 'next-error)
(define-key ctl-o-map "\C-o" 'dabbrev-expand)
(define-key ctl-o-map "\C-p" 'previous-error)

;;; Comment insertion
(define-key ctl-o-map "-" 'insert-dash-comment)
(define-key ctl-o-map "*" 'insert-star-comment)
(define-key ctl-o-map "=" 'insert-equal-comment)
(define-key ctl-o-map " " 'insert-space-comment)
(define-key ctl-o-map ";" 'kill-comment)
(define-key ctl-o-map [?\C-5] 'insert-percent-comment)

(define-key ctl-o-map "{" 'c-set-style)
