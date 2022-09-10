(provide 'keydefs)

;;; ------------------------------------------------------------------------
;;;
;;; Control-O map
;;;
;;; ------------------------------------------------------------------------

(defvar ctl-o-map (make-sparse-keymap)
  "Keymap for fast access to programmers commands.")
(progn (defalias 'ctl-o-prefix ctl-o-map)
       (define-key global-map "\C-o" 'ctl-o-prefix)
       (define-key global-map "\M-o" 'ctl-o-prefix))


(define-key ctl-o-map "a" 'align)
(define-key ctl-o-map "b" 'gdb)
(define-key ctl-o-map "c" 'compile)
(define-key ctl-o-map "d" 'c-indent-define)
(define-key ctl-o-map "e" 'emerge-files)
(define-key ctl-o-map "f" 'vc-git-grep)
(define-key ctl-o-map "g" 'manygrep)
(define-key ctl-o-map "h" 'insert-xlr-header)
(define-key ctl-o-map "i" 'overwrite-mode)
(define-key ctl-o-map "j" 'grep)
(define-key ctl-o-map "l" 'goto-line)
(define-key ctl-o-map "m" 'man)
(define-key ctl-o-map "n" 'gnus)
(define-key ctl-o-map "p" 'toggle-truncate-lines)
(define-key ctl-o-map "q" 'insert-gpl-header)
(define-key ctl-o-map "r" 'insert-reviewed-by)
(define-key ctl-o-map "s" 'shell)
(define-key ctl-o-map "t" 'tabify)
(define-key ctl-o-map "y" 'rotate-yank-pointer)
(define-key ctl-o-map "\\" 'c-backslash-region)

(define-key ctl-o-map "\C-a" 'insert-acked-by)
(define-key ctl-o-map "\C-b" 'lldb)
(define-key ctl-o-map "\C-c" 'clang-format-buffer)
(define-key ctl-o-map "\C-f" 'fill-region)
(define-key ctl-o-map "\C-g" 'projectile-ag)
(define-key ctl-o-map "\C-l" 'font-lock-fontify-buffer)
(define-key ctl-o-map "\C-n" 'next-error)
(define-key ctl-o-map "\C-m" 'mu4e)
(define-key ctl-o-map "\C-o" 'dabbrev-expand)
(define-key ctl-o-map "\C-p" 'previous-error)
(define-key ctl-o-map "\C-r" 'delete-trailing-whitespace)
(define-key ctl-o-map "\C-s" 'magit)
(define-key ctl-o-map "\C-t" 'untabify)
(global-set-key "\M-\t" 'clang-format)


(define-key ctl-o-map "["    'checkout-curr)
(define-key ctl-o-map "]"    'checkin-curr)

;;; Comment insertion
(define-key ctl-o-map "-" 'insert-dash-comment)
(define-key ctl-o-map "*" 'insert-star-comment)
(define-key ctl-o-map "=" 'insert-equal-comment)
(define-key ctl-o-map "/" 'use-cplusplus-style-comments)
(define-key ctl-o-map " " 'insert-space-comment)
(define-key ctl-o-map ";" 'kill-comment)

;;; Tab width
(define-key ctl-o-map "2" 'set-tab-width-to-2)
(define-key ctl-o-map "3" 'set-tab-width-to-3)
(define-key ctl-o-map "4" 'set-tab-width-to-4)
(define-key ctl-o-map "8" 'set-tab-width-to-8)

;;;(or (emacs-18-p) (define-key ctl-o-map [?\C-5] 'insert-percent-comment))

(define-key ctl-o-map "{" 'c-set-style)

;;;
;;;
;;;  Key defaults
;;;
;;;
(global-set-key [break] 'undo)
(global-set-key [f1] 'undo)

(global-set-key [f2] 'jump-to-register)
(global-set-key [(shift f2)] 'point-to-register)

(global-set-key [f3] 'other-window)

(global-set-key [f4] 'next-error)
(global-set-key [(shift f4)] 'previous-error)
(global-set-key [(control f4)] 'previous-error)
(global-set-key [(meta f4)] 'previous-error)

(global-set-key [f5] 'rotate-yank-pointer)
(global-set-key [(control f5)] 'clang-format-buffer)

(global-set-key [f6] 'switch-to-buffer)

(global-set-key [f7] 'call-last-kbd-macro)

(global-set-key [f8] 'dabbrev-expand)

(global-set-key [control f9] 'projectile-ag)
(global-set-key [f9] 'gud-step) ; f9
(global-set-key [f10] 'gud-next) ; f10
(global-set-key [(control f10)] 'gud-step) ; f10

(global-set-key [f12] 'dabbrev-expand)
(global-set-key [(control f12)] 'cycle-colors)
(global-set-key [(meta f12)] 'cycle-colors)


(defun insert-acked-by ()
  "Insert Acked-by message"
  (interactive)
  (insert "Acked-by: Christophe de Dinechin <dinechin@redhat.com>"))
(defun insert-reviewed-by ()
  "Insert Reviewed-by message"
  (interactive)
  (insert "Reviewed-by: Christophe de Dinechin <dinechin@redhat.com>"))
(defun insert-signed-off-by ()
  "Insert Reviewed-by message"
  (interactive)
  (insert "Signed-off-by: Christophe de Dinechin <dinechin@redhat.com>"))

;;; When using a raw Emacs (no windows) on an xterm, remap the BackSpace key
(or (and window-system (member (getenv "TERM") '("xterm" "xterms" "vt100" "hpterm")))
    (add-hook 'term-setup-hook 'remap-xterm-bksp))

(defun remap-xterm-bksp ()
  "Remap the Backspace and DEL keys for an xterm or a telnet vt100"
  (keyboard-translate ?\C-h ?\C-?) ; Backspace key is now delete ...
  (keyboard-translate ?\C-\\ ?\C-h) ; Ctrl-? is now help ...
)
;(defun remap-xterm-bksp ()
;  "Remap the Backspace and DEL keys for an xterm or a telnet vt100"
;  (define-key global-map "\C-h" 'backward-delete-char-untabify)
;  (define-key global-map [DEL] 'delete-char))

(require 'ansi-color)
(defun ansi-color-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(defun ansi-color-region ()
  (interactive)
  (ansi-color-apply-on-region (point) (mark)))
