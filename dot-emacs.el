; Christophe de Dinechin's Emacs Initializations

;; Options Menu Settings
;; =====================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "~/.xemacs-options")
  (load-file "~/.xemacs")))
;; ============================
;; End of Options Menu Settings

(setq load-path (append '("~/.emacs-lisp"
                          "/usr/local/share/clang"
                          "/usr/local/share/emacs/site-lisp") load-path))
(setq exec-path (append exec-path (list "/opt/local/bin" "/usr/local/bin")))

;; Default setenv
(defun setenv-compiled ()
  (interactive)
  (setenv "BUILDENV" "macosx-clang")
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH") ":/usr/local/bin"))
  (setenv "PKG_CONFIG_PATH" "//usr/local/opt/jpeg-turbo/lib/pkgconfig:/usr/local/Cellar/openssl/1.0.2l/lib/pkgconfig:/usr/local/share/pkgconfig:/usr/local/lib/pkgconfig"))
(setenv-compiled)

(require 'cdefs)
(require 'keydefs)
(require 'findgrep)
(require 'comments)
(require 'checkout)
(require 'ia64-asm-mode)
(require 'xl)
(require 'defaults)
(require 'blogmax)
(require 'git)
(require 'vc-git)
(require 'git-blame)
(require 'xcscope)
(cscope-setup)
(require 'php-mode)
(require 'chronometer)
(require 'lldb)
(require 'json-pretty-print)
(require 'gnus-article-treat-patch)
(require 'clang-format)
(load "~/.emacs-lisp/haskell-mode-2.4/haskell-site-file")
(load "~/.emacs-lisp/mu4e-setup.el")
(load "~/.emacs-lisp/mu4e-jump-to-list.el")

;;(set-background-color "Wheat")
(set-background-color "#E0E0FE")
(set-foreground-color "Black")
(set-cursor-color "Orange")
(set-mouse-color "Blue")
(set-default 'cursor-type 'box)
(set-face-background 'region "yellow")
(set-face-foreground 'region "blue")
(tool-bar-mode nil)

(set-variable 'manygrep-topdir "/Users/ddd/Work/spice/spice-gtk")
(set-variable 'grep-command "grep -in ")
(set-variable 'compile-command
              "cd /Users/ddd/Work/spice; make --print-directory debug")
(set-variable 'mac-pass-option-to-system nil)
(setq default-frame-font "fontset-mac")
;; (set-default-frame-alist)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load hilt19, but not under xemacs
(load-library "font-lock")
(global-font-lock-mode t)
(setq c-default-style "ddd")
;(setq gnus-default-nntp-server "news.free.fr")
(setq gnus-default-nntp-server "news.gmane.org")

;; Check auto-compression, add files that are really gzipped
(require 'jka-compr)
(add-to-list 'jka-compr-compression-info-list
             ["\\.duf\\'"
              "compressing" "gzip" ("-c" "-q")
              "uncompressing" "gzip" ("-c" "-q" "-d")
              t nil "\037\213"])
(jka-compr-install)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; Colorize compilation buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;==============================================================================
;
;  Custom variables
;
;==============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(package-selected-packages (quote (rust-mode adoc-mode markdown-mode)))
 '(safe-local-variable-values
   (quote
    ((c-indent-level . 8)
     (whitespace-check-buffer-indent)
     (eval ignore-errors
           (require
            (quote whitespace))
           (whitespace-mode 1))
     (whitespace-line-column . 79)
     (whitespace-style face indentation)
     (eval progn
           (c-set-offset
            (quote case-label)
            (quote 0))
           (c-set-offset
            (quote innamespace)
            (quote 0))
           (c-set-offset
            (quote inline-open)
            (quote 0))))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
