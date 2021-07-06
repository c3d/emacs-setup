; Christophe de Dinechin's Emacs Initializations

;; Options Menu Settings
;; =====================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(if (< emacs-major-version 27) (package-initialize))

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
                          "~/.emacs-lisp/lisp/progmodes"
                          "/usr/local/share/clang"
                          "/opt/local/libexec/llvm-10/libexec/clang-format"
                          "/usr/local/share/emacs/site-lisp")
                        load-path))
(setq exec-path (append exec-path (list "/opt/local/bin" "/usr/local/bin")))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Default setenv
(defun setenv-compiled ()
  (interactive)
  (setenv "BUILDENV" "macosx-clang")
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH") ":/usr/local/bin:/usr/local/opt/qt/bin"))
  (setenv "PKG_CONFIG_PATH" "/usr/local/share/pkgconfig:/usr/local/lib/pkgconfig"))
(setenv-compiled)

(require 'cdefs)
(require 'keydefs)
(require 'findgrep)
(require 'comments)
(require 'ia64-asm-mode)
(require 'xl)
(require 'defaults)
(require 'blogmax)
(require 'magit)
(require 'vc)
(require 'vc-git)
(require 'git-blame)
(require 'xcscope)
; (cscope-setup)
(add-hook 'go-mode-hook (function cscope-minor-mode))
(require 'php-mode)
(require 'gnus-article-treat-patch)
(require 'clang-format)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)

; (load "~/.emacs-lisp/haskell-mode-2.4/haskell-site-file")
(load "~/.emacs-lisp/mu4e-setup.el")
(load "~/.emacs-lisp/lisp/progmodes/gud.el")

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
(setq-default fill-column 80)

;; Load hilt19, but not under xemacs
(load-library "font-lock")
(global-font-lock-mode t)
(setq c-default-style "ddd")
;(setq gnus-default-nntp-server "news.free.fr")
(setq gnus-default-nntp-server "news.gmane.io")

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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



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
 '(package-selected-packages
   '(rust-mode markdown-mode flymake-json mu4e-views go-guru magit-gh-pulls gh meson-mode realgud-lldb checkbox git php-mode xcscope ag go-mode imenus imenu-list imenu-anywhere magit adoc-mode))
 '(safe-local-variable-values
   '((c-indent-level . 8)
     (whitespace-check-buffer-indent)
     (eval ignore-errors
           (require 'whitespace)
           (whitespace-mode 1))
     (whitespace-line-column . 79)
     (whitespace-style face indentation)
     (eval progn
           (c-set-offset 'case-label '0)
           (c-set-offset 'innamespace '0)
           (c-set-offset 'inline-open '0))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
