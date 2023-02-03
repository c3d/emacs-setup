; Christophe de Dinechin's Emacs Initializations

;; Custom configuration
(set-variable 'manygrep-topdir "/Users/ddd/Work/spice/spice-gtk")
(set-variable 'grep-command "grep -in ")
(set-variable 'compile-command "cd ~/Work/xl && make")
(set-variable 'mac-pass-option-to-system nil)
(setq default-frame-font "fontset-mac")


;; Where to look for Emacs code
(setq load-path (append '("~/.emacs-lisp"
                          "~/.emacs-lisp/lisp/progmodes"
                          "/usr/local/share/clang"
                          "/opt/homebrew/share/clang"
                          "/opt/local/libexec/llvm-10/libexec/clang-format"
                          "/usr/local/share/emacs/site-lisp")
                        load-path))
(setq exec-path (append exec-path (list "/opt/local/bin" "/usr/local/bin" "~/.cargo/bin")))


;; My personal stuff (in .emacs-lisp, not fetched from packages)
(require 'cdefs)
(require 'keydefs)
(require 'findgrep)
(require 'comments)
(require 'ia64-asm-mode)
(require 'xl)
(require 'defaults)
(require 'blogmax)
(require 'gnus-article-treat-patch)
(require 'mu4e-setup)
(require 'gud)

;; Packages from the world
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;(use-package auto-package-update
;  :config
;  (setq auto-package-update-delete-old-versions t)
;  (setq auto-package-update-hide-results t)
;  (auto-package-update-maybe))
(use-package adoc-mode)
(use-package ag)
(use-package auto-complete)
(use-package ac-clang)
; (use-package ansi-color)
(use-package checkbox)
(use-package clang-format :ensure nil) ;; This one comes from LLVM, see above
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))
(use-package flycheck
  :ensure
  :config
  (setq flycheck-standard-error-navigation nil))
(use-package flymake-json)
(use-package forge :after magit)
(use-package forge)
(use-package git)
(use-package go-guru)
(use-package go-mode)
(use-package imenu-anywhere)
(use-package imenu-list)
(use-package imenus)
                                        ; (use-package jka-compr)
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
(use-package magit)
(use-package markdown-mode)
(use-package meson-mode)
;; (use-package mu4e-views)
(use-package php-mode)
(use-package projectile)
; (use-package realgud-lldb)
; (use-package rust-mode)
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-o c" . rustic-compile)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-compile-command "make -C ~/Work/ociplex all")
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

; (use-package vc)
(use-package xcscope
  :config
  (cscope-setup))
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; Recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)

;;(set-background-color "Wheat")
(set-background-color "#E0E0FE")
(set-foreground-color "Black")
(set-cursor-color "Orange")
(set-mouse-color "Blue")
(set-default 'cursor-type 'box)
(set-face-background 'region "yellow")
(set-face-foreground 'region "blue")
(tool-bar-mode nil)

;; Always strip whitespace (even if that gets me in trouble during code reviews)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 80)
(setq c-default-style "ddd")
(setq gnus-default-nntp-server "news.gmane.io")
(setq gnus-select-method '(nntp "news.gmane.io"))

;; Check auto-compression, add files that are really gzipped
(add-to-list 'jka-compr-compression-info-list
             ["\\.duf\\'"
              "compressing" "gzip" ("-c" "-q")
              "uncompressing" "gzip" ("-c" "-q" "-d")
              t nil "\037\213"])
(auto-compression-mode 1)
; (jka-compr-install)

;; Make shell scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Colorize compilation buffers
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Projectile mode
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
   '(ement ac-clang clang-format flycheck-rust flycheck-ycmd lsp-ui lsp-mode rustic unfill realgud-lldb xcscope use-package rust-mode projectile php-mode mu4e-views meson-mode magit-popup magit-gh-pulls imenus imenu-list imenu-anywhere graphql go-guru git forge flymake-json checkbox async ag adoc-mode))
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
(put 'magit-diff-edit-hunk-commit 'disabled nil)
